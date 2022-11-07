//! Flags
//!
//! A simple command line parser with minimalistic API.
//!
use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;
use std::string::ToString;

///
/// FlagErrorKind is a type that represents various flag error types.
///
#[derive(Debug, PartialEq, Eq)]
pub enum FlagErrorKind {
    IncorrectNumberOfDashes,
    UnrecognizedFlagName,
    IsAValue,
    UsageError,
    InvalidBooleanValue,
    MissingRequiredValue,
}

impl fmt::Display for FlagErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlagErrorKind::InvalidBooleanValue => f.write_str("InvalidBooleanValue"),
            FlagErrorKind::UnrecognizedFlagName => f.write_str("UnrecognizedFlagName"),
            FlagErrorKind::IsAValue => f.write_str("IsAValue"),
            FlagErrorKind::UsageError => f.write_str("Usage error"),
            FlagErrorKind::MissingRequiredValue => f.write_str("MissingRequiredValue"),
            FlagErrorKind::IncorrectNumberOfDashes => f.write_str("IncorrectNumberOfDashes"),
        }
    }
}

///
/// FlagError holds the error type and a message indicating the reason of the error.
///
#[derive(Debug)]
pub struct FlagError {
    pub error_type: FlagErrorKind,
    pub message: String,
}

impl fmt::Display for FlagError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("{}: {}", self.error_type, self.message))
    }
}

///
/// Flag is an opaque structure that represents a single command-line argument / flag.
///
pub struct Flag {
    shortname: Option<String>,
    longname: Option<String>,
    description: String,
    default_value: Option<Box<dyn Any>>,
    mandatory: bool,
    value_type: TypeId,
    value_unparsed: Option<String>,
}

impl Flag {
    ///
    /// Returns a new `Flag` initialized with the values provided.
    ///
    pub fn new(
        shortname: Option<&str>,
        longname: Option<&str>,
        description: &str,
        mandatory: bool,
        value_type: TypeId,
        default_value: Option<Box<dyn Any>>,
    ) -> Rc<RefCell<Self>> {
        let s = shortname.map_or_else(|| None, |s| Some(s.to_string()));
        let l = longname.map_or_else(|| None, |s| Some(s.to_string()));
        let description = description.to_string();
        Rc::new(RefCell::new(Self {
            shortname: s,
            longname: l,
            description,
            default_value,
            mandatory,
            value_type,
            value_unparsed: None,
        }))
    }

    ///
    /// `kind` is used to indicate the type of the value being stored in the Flag.
    ///
    pub fn kind<T: 'static>() -> TypeId {
        TypeId::of::<T>()
    }

    ///
    /// Returns `Some(&String)` containing the shortname of the flag or `None`.
    ///
    pub fn shortname(&self) -> Option<&String> {
        self.shortname.as_ref()
    }

    ///
    /// Returns `Some(&String)` containing the longname of the flag or `None`.
    ///
    pub fn longname(&self) -> Option<&String> {
        self.longname.as_ref()
    }

    ///
    /// Returns a reference to the description string of the flag.
    ///
    pub fn description(&self) -> &String {
        &self.description
    }

    ///
    /// Returns `true` or `false` indicating if the flag is mandatory/optional.
    ///
    pub fn mandatory(&self) -> bool {
        self.mandatory
    }

    ///
    /// Returns the `TypeId` of the value contained in the flag.
    ///
    pub fn value_type(&self) -> TypeId {
        self.value_type
    }

    ///
    /// A generic method on type `V` (for Value) to retrieve the value in the Flag. The method
    /// returns an error if the value could not be parsed into the value of type `V`. Upon
    /// successful parsing `Result` contains the value of type `V`.
    ///
    /// # Panics
    ///
    /// Panics if the flag does not contain a value or a default value.
    ///
    ///
    pub fn get_value<V: 'static + FromStr + Clone>(&self) -> Result<V, String> {
        let value_str = self.get_value_unparsed();
        match value_str {
            Some(value_str) => {
                let r = value_str[..].parse::<V>();
                match r {
                    Ok(v) => Ok(v),
                    Err(_) => Err(format!(
                        "value {} cannot be parsed into type {}",
                        value_str,
                        std::any::type_name::<V>()
                    )),
                }
            }
            None => {
                if let Some(v) = self.get_default_value::<V>() {
                    return Ok(v);
                }
                panic!("value not available and the default value was not set");
            }
        }
    }

    fn get_default_value<T: Clone + 'static>(&self) -> Option<T> {
        let t = TypeId::of::<T>();
        if self.value_type != t {
            panic!("flag value type does not match {:?}", t);
        }
        let v = self.default_value.as_ref();
        if v.is_none() {
            return None;
        }
        let v = v.unwrap();
        let t = v.downcast_ref::<T>();
        let t = t.expect("unexpected error unwrapping default value");
        let t = (*t).clone();
        Some(t)
    }

    ///
    /// The method returns `Some(String)` containing the string representation of the default
    /// value. Or `None` if the value could not be converted to a string.
    ///
    pub fn try_default_value_to_string(&self) -> Option<String> {
        if let Some(dv) = self.default_value_to_string::<&str>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<String>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<i32>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<u32>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<i64>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<u64>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<f32>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<f64>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<char>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<bool>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<i8>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<u8>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<i128>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<u128>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<isize>() {
            return Some(dv);
        }
        if let Some(dv) = self.default_value_to_string::<usize>() {
            return Some(dv);
        }
        return None;
    }

    fn default_value_to_string<V: 'static + Clone + ToString>(&self) -> Option<String> {
        if TypeId::of::<V>() == self.value_type {
            let msg = format!(
                "unexpected error when unwrapping {} default value",
                std::any::type_name::<V>()
            );
            return Some(self.get_default_value::<V>().expect(&msg).to_string());
        }
        return None;
    }

    fn get_value_unparsed(&self) -> Option<&String> {
        self.value_unparsed.as_ref()
    }

    fn set_value_unparsed(&mut self, s: Option<String>) {
        self.value_unparsed = s;
    }
}

impl fmt::Display for Flag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut usage = String::new();

        if let (Some(shortname), Some(longname)) = (self.shortname(), self.longname()) {
            usage.push_str(&format!("{} {}", shortname, longname));
        } else if let Some(shortname) = self.shortname() {
            usage.push_str(shortname);
        } else if let Some(longname) = self.longname() {
            usage.push_str(longname);
        }
        if self.value_type == TypeId::of::<bool>() {
            usage.push_str(" ");
        } else {
            if self.mandatory() {
                usage.push_str(" <value> ");
            } else {
                usage.push_str(" [value] ");
            }
        }
        usage.push_str(&format!("{} ", self.description()));
        if let Some(default_value) = self.try_default_value_to_string() {
            usage.push_str(&format!("(default: {})", default_value));
        }
        f.write_str(&usage)
    }
}

///
/// A FlagSet is an opaque structure that represents/holds all the flags relevant for the command.
///
/// Printing the `FlagSet` displays the usage.
///
pub struct FlagSet {
    flags: Vec<(String, String)>,
    mandatory_flags: Vec<(String, String)>,
    flag_map: HashMap<String, Rc<RefCell<Flag>>>,
    indent: usize,
}

impl fmt::Display for FlagSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut usage = String::new();
        usage.push_str("\n");
        for (s, l) in self.flags.iter() {
            let has_shortname = !s.is_empty();
            let has_longname = !l.is_empty();
            let mut usage_len = 0;
            if has_shortname && has_longname {
                let fmt_str = &format!("{} {}", s, l);
                usage.push_str(fmt_str);
                usage_len += fmt_str.len();
            } else if has_shortname && !has_longname {
                let fmt_str = &format!("{}", s);
                usage.push_str(fmt_str);
                usage_len += fmt_str.len();
            } else {
                let fmt_str = &format!("{}", l);
                usage.push_str(fmt_str);
                usage_len += fmt_str.len();
            }
            let flag;
            if has_shortname {
                flag = self.flag_map.get(s);
            } else {
                flag = self.flag_map.get(l);
            }
            if let Some(flag) = flag {
                if flag.borrow().value_type() != TypeId::of::<bool>() {
                    if flag.borrow().mandatory() {
                        let fmt_str = &format!(" {} ", "<value>");
                        usage.push_str(fmt_str);
                        usage_len += fmt_str.len();
                    } else {
                        let fmt_str = &format!(" {} ", "[value]");
                        usage.push_str(fmt_str);
                        usage_len += fmt_str.len();
                    }
                }
                let description = flag.borrow().description().clone();
                usage.push_str(&format!("{:indent$}", "", indent = self.indent - usage_len));
                usage.push_str(&format!("{}", description));
                if let Some(default_value) = flag.borrow().try_default_value_to_string() {
                    usage.push_str(&format!(" (default: {})", default_value));
                } else {
                    usage.push_str(&format!(" (default: error)"));
                }
            }
            usage.push_str("\n");
        }
        f.write_str(&usage)
    }
}

impl FlagSet {
    ///
    /// Returns a new `FlagSet`
    ///
    pub fn new() -> Self {
        let flag_map = HashMap::new();
        let flags: Vec<(String, String)> = vec![];
        let mandatory_flags: Vec<(String, String)> = vec![];
        Self {
            flags,
            mandatory_flags,
            flag_map,
            indent: 0,
        }
    }

    ///
    /// Prints the usage of all the flags in the flagset to stdout
    ///
    pub fn usage(&self) {
        println!("{}", self);
    }

    ///
    /// Method adds a given flag to the flagset.
    ///
    /// # Panics
    ///
    /// The method panics if the flag is missing both the short and long flag names.
    ///
    pub fn add(&mut self, f: &Rc<RefCell<Flag>>) {
        let flag = f.borrow();
        let shortname = flag.shortname();
        let longname = flag.longname();

        if shortname == None && longname == None {
            panic!("required: short name or long name");
        }

        let mut value_pad: usize = 0;
        if f.borrow().value_type() != TypeId::of::<bool>() {
            value_pad = " <value> ".len();
        }

        if let (Some(shortname), Some(longname)) = (shortname, longname) {
            self.flags.push((shortname.clone(), longname.clone()));
            if flag.mandatory() {
                self.mandatory_flags
                    .push((shortname.clone(), longname.clone()));
            }
            self.flag_map.insert(shortname.clone(), Rc::clone(f));
            let v = shortname.len() + 1 + longname.len() + value_pad;
            if v > self.indent {
                self.indent = v;
            }
            return;
        }
        if shortname.is_some() {
            let shortname = shortname.expect("missing short flag name");
            self.flag_map.insert(shortname.clone(), Rc::clone(f));
            self.flags.push((shortname.clone(), "".to_string()));
            if flag.mandatory() {
                self.mandatory_flags
                    .push((shortname.clone(), "".to_string()));
            }
            let v = shortname.len() + value_pad;
            if v > self.indent {
                self.indent = v;
            }
            return;
        }
        if longname.is_some() {
            let longname = longname.expect("missing long flag name");
            self.flag_map.insert(longname.clone(), Rc::clone(f));
            self.flags.push((longname.clone(), "".to_string()));
            if flag.mandatory() {
                self.mandatory_flags
                    .push((longname.clone(), "".to_string()));
            }
            let v = longname.len() + value_pad;
            if v > self.indent {
                self.indent = v;
            }
        }
    }

    ///
    /// The method parses the commmand-line arguments. The method returns `FlagError` if it
    /// failed to parse the arguments to the specified flags.
    ///
    /// The method expects the command-line arguments passed-in as-is from `std::env::args()`.
    ///
    pub fn parse(&mut self, args: &mut env::Args) -> Result<(), FlagError> {
        args.next(); // skip the program name.
        self.parse_args(args.collect::<Vec<String>>())
    }

    fn set_flag_value_unparsed(&mut self, flag_name: &String, flag_value: String) {
        let flag = self.flag_map.get(flag_name);
        let flag = flag.expect("unexpected error: flag not found");
        let mut flag = flag.borrow_mut();
        flag.set_value_unparsed(Some(flag_value));
    }

    fn has_mandatory_flags(&self) -> bool {
        for (_, flag) in self.flag_map.iter() {
            if flag.borrow().mandatory() {
                return true;
            }
        }
        return false;
    }

    fn parse_args(&mut self, args: Vec<String>) -> Result<(), FlagError> {
        let mut args = args.iter();
        let mut flag_name = String::new();
        let mut is_value: bool = false;
        let mut flag_names = vec![];
        loop {
            if let Some(arg) = args.next() {
                if let Some(flag_error) = self.check_arg(arg).err() {
                    if flag_name.is_empty() {
                        if self.has_mandatory_flags() {
                            return Err(FlagError {
                                error_type: FlagErrorKind::UsageError,
                                message: flag_error.message,
                            });
                        } else {
                            return Ok(());
                        }
                    } else {
                        match flag_error.error_type {
                            FlagErrorKind::IsAValue => {
                                is_value = true;
                            }
                            _ => {
                                return Err(FlagError {
                                    error_type: FlagErrorKind::UsageError,
                                    message: flag_error.message,
                                });
                            }
                        }
                    }
                }
                // no error with the argument and flag is empty
                if flag_name.is_empty() && !is_value {
                    // if it is not a value
                    flag_name.push_str(arg);
                    flag_names.push(arg);
                    continue;
                }

                if !flag_name.is_empty() {
                    let value_type: TypeId;
                    {
                        let flag = self.flag_map.get(&flag_name);
                        let flag = flag.expect("unexpected error: flag not found");
                        let flag = flag.borrow();
                        value_type = flag.value_type();
                    }
                    if is_value {
                        if TypeId::of::<bool>() == value_type {
                            if arg.eq_ignore_ascii_case("true") || arg.eq_ignore_ascii_case("false")
                            {
                                self.set_flag_value_unparsed(
                                    &flag_name,
                                    arg.clone().to_lowercase(),
                                );
                                flag_name.clear();
                            } else {
                                return Err(FlagError {
                                    error_type: FlagErrorKind::UsageError,
                                    message: format!("invalid boolean value"),
                                });
                            }
                        } else {
                            self.set_flag_value_unparsed(&flag_name, arg.clone());
                            flag_name.clear();
                        }
                    } else {
                        // is not a value (arg is the next flag).
                        // check if
                        if TypeId::of::<bool>() == value_type {
                            self.set_flag_value_unparsed(&flag_name, "true".to_string());
                            flag_name.clear();
                        } else {
                            // if a flag is specified and no value then always return an error
                            return Err(FlagError {
                                error_type: FlagErrorKind::MissingRequiredValue,
                                message: format!("missing required value for {}", flag_name),
                            });
                        }
                    }
                }
            } else {
                break;
            }
        }
        if !self.mandatory_flags.is_empty() {
            let mut count = 0;
            for flag_name in flag_names {
                if self
                    .mandatory_flags
                    .iter()
                    .find(|(s, l)| s == flag_name || l == flag_name)
                    .is_some()
                {
                    count += 1;
                }
            }
            if count != self.mandatory_flags.len() {
                return Err(FlagError {
                    error_type: FlagErrorKind::MissingRequiredValue,
                    message: "missing mandatory flags".to_string(),
                });
            }
        }
        Ok(())
    }

    // check if the argument is defined and flag name is in valid format
    fn check_arg(&self, arg: &String) -> Result<(), FlagError> {
        if !arg.starts_with("-") {
            return Err(FlagError {
                error_type: FlagErrorKind::IsAValue,
                message: format!("is a value"),
            });
        }
        if !self.is_valid_prefix(arg) {
            return Err(FlagError {
                error_type: FlagErrorKind::IncorrectNumberOfDashes,
                message: format!(
                    "{} has incorrect number of dashes (use -/-- for short/long name respectively)",
                    arg
                ),
            });
        }
        let flag = self.flag_map.get(arg);
        if flag.is_none() {
            return Err(FlagError {
                error_type: FlagErrorKind::UnrecognizedFlagName,
                message: format!("{} is not defined or added to the flagset", arg),
            });
        }
        Ok(())
    }

    fn is_valid_prefix(&self, arg: &String) -> bool {
        let num_dashes = self.num_dashes(&arg);
        (num_dashes == 1 && arg.len() == 2) || (num_dashes == 2 && arg.len() > 3)
    }

    fn num_dashes(&self, arg: &String) -> i32 {
        let chars = arg.chars();
        let mut num_dashes = 0;
        for c in chars {
            if c.eq(&'-') {
                num_dashes += 1;
            } else {
                break;
            }
        }
        num_dashes
    }
}

#[cfg(test)]
mod tests {
    use crate::{Flag, FlagErrorKind, FlagSet};

    #[test]
    fn make_new_flag() {
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(3i32)),
        );
        assert_eq!(retry_flag.borrow().shortname(), Some(&"-r".to_string()));
        assert_eq!(retry_flag.borrow().longname(), Some(&"--retry".to_string()));
        assert_eq!(
            retry_flag.borrow().description(),
            &"number of retry operations".to_string()
        );
        assert_eq!(retry_flag.borrow().mandatory(), false);
    }

    #[test]
    #[should_panic(expected = "required: short name or long name")]
    fn adding_invalid_flag_should_panic() {
        let retry_flag = Flag::new(
            None,
            None,
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(3i32)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&retry_flag);
    }

    #[test]
    fn cmdarg_passed_flag_with_value_must_return_value() {
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(3i32)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&retry_flag);
        let args = vec!["-r", "10"];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        match flagset.parse_args(args) {
            Ok(_) => {}
            Err(_) => assert!(
                false,
                "failed: should have been able to read flag with value {}",
                10
            ),
        }
        let v = retry_flag.borrow().get_value::<i32>();
        match v {
            Ok(v) => assert_eq!(v, 10),
            Err(e) => assert!(false, "{}", e),
        }
    }

    #[test]
    fn cmdarg_empty_all_optional_flags_must_return_default_value() {
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(3i32)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&retry_flag);
        let args: Vec<&str> = vec![];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        match flagset.parse_args(args) {
            Ok(_) => {}
            Err(e) => {
                assert!(
                    false,
                    "no mandatory flags; no errors must have been be reported {:?}",
                    e
                );
            }
        }
        let v = retry_flag.borrow().get_value::<i32>();
        match v {
            Ok(v) => assert_eq!(v, 3),
            Err(e) => assert!(false, "{}", e),
        }
    }

    #[test]
    fn cmdarg_value_not_passed_must_error() {
        let bflag = Flag::new(
            Some("-b"),
            Some("--backup-path"),
            "path to the directory that can hold the backup files",
            true,
            Flag::kind::<String>(),
            Some(Box::new("/root/backup/10102022".to_string())),
        );
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(3i32)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&bflag);
        flagset.add(&retry_flag);
        let args = vec!["-b", "-r", "15"];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        match flagset.parse_args(args).err() {
            Some(v) => {
                assert_eq!(FlagErrorKind::MissingRequiredValue, v.error_type);
                assert_eq!(v.message, format!("missing required value for -b"));
            }
            None => {
                assert!(
                    false,
                    "a non-boolean flag type should be provided with a value"
                );
            }
        }
    }

    #[test]
    fn cmdarg_bool_flag_ok() {
        let bflag = Flag::new(
            Some("-b"),
            Some("--backup-path"),
            "path to the directory that can hold the backup files",
            true,
            Flag::kind::<String>(),
            Some(Box::new("/root/backup/10102022".to_string())),
        );
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(3i32)),
        );
        let force_flag = Flag::new(
            Some("-f"),
            Some("--force"),
            "force the operation",
            false,
            Flag::kind::<bool>(),
            Some(Box::new(false)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&bflag);
        flagset.add(&retry_flag);
        flagset.add(&force_flag);
        let args = vec!["-r", "15", "--force"];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        match flagset.parse_args(args).err() {
            Some(_) => {}
            None => {
                assert!(false, "unexpected error: flags correct");
            }
        }
        let bval = bflag
            .borrow()
            .get_value::<String>()
            .expect("expected to receive default value");
        assert_eq!(bval, String::from("/root/backup/10102022".to_string()));

        let rval = retry_flag
            .borrow()
            .get_value::<i32>()
            .expect("expected to receive passed in value");
        assert_eq!(rval, 15);

        let fval = force_flag
            .borrow()
            .get_value::<bool>()
            .expect("expect default value");
        assert_eq!(fval, false);
    }

    #[test]
    #[should_panic(expected = "value not available and the default value was not set")]
    fn read_optional_flag_with_no_default_value() {
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            None,
        );
        let mut flagset = FlagSet::new();
        flagset.add(&retry_flag);
        let args: Vec<&str> = vec![];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        assert_eq!(flagset.parse_args(args).err().is_none(), true);
        let _rval = retry_flag.borrow().get_value::<i32>().unwrap();
    }

    #[test]
    #[should_panic(expected = "flag value type does not match")]
    fn read_flag_with_different_kind_must_panic() {
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(5i32)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&retry_flag);
        let args: Vec<&str> = vec!["--retry 10"];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        assert_eq!(flagset.parse_args(args).err().is_none(), true);
        // reading i32 as f64 will panic
        let _rval = retry_flag.borrow().get_value::<f64>().unwrap();
    }

    #[test]
    fn cmdarg_boolean_values_can_also_have_true_false() {
        let force_flag = Flag::new(
            Some("-f"),
            Some("--force"),
            "force the operation",
            false,
            Flag::kind::<bool>(),
            Some(Box::new(false)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&force_flag);
        let args = vec!["-f", "True"];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        match flagset.parse_args(args).err() {
            Some(e) => {
                assert_eq!("", e.message, "unexpected error: flags are correct");
            }
            None => {}
        }
        let fval = force_flag
            .borrow()
            .get_value::<bool>()
            .expect("expect default value");
        assert_eq!(fval, true);
    }

    #[test]
    fn boolean_flag_formatter() {
        let force_flag = Flag::new(
            Some("-f"),
            Some("--force"),
            "force the operation",
            false,
            Flag::kind::<bool>(),
            Some(Box::new(false)),
        );
        let force_str = "-f --force force the operation (default: false)";
        assert_eq!(format!("{}", force_flag.borrow()), force_str);
    }

    #[test]
    fn non_boolean_mandatory_flag_formatter() {
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            true,
            Flag::kind::<i32>(),
            Some(Box::new(5i32)),
        );
        let retry_str = "-r --retry <value> number of retry operations (default: 5)";
        assert_eq!(format!("{}", retry_flag.borrow()), retry_str);
    }

    #[test]
    fn flagset_usage() {
        let bflag = Flag::new(
            Some("-b"),
            Some("--backup-path"),
            "path to the directory that can hold the backup files",
            true,
            Flag::kind::<String>(),
            Some(Box::new("/root/backup/10102022".to_string())),
        );
        let retry_flag = Flag::new(
            Some("-r"),
            Some("--retry"),
            "number of retry operations",
            false,
            Flag::kind::<i32>(),
            Some(Box::new(3i32)),
        );
        let force_flag = Flag::new(
            Some("-f"),
            Some("--force"),
            "force the operation",
            false,
            Flag::kind::<bool>(),
            Some(Box::new(false)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&bflag);
        flagset.add(&retry_flag);
        flagset.add(&force_flag);
        let usage_str = "
-b --backup-path <value> path to the directory that can hold the backup files (default: /root/backup/10102022)
-r --retry [value]       number of retry operations (default: 3)
-f --force               force the operation (default: false)
";
        assert_eq!(usage_str, format!("{}", flagset));
    }
}
