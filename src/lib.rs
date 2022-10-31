use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;
use std::string::ToString;

static SHORT_FLAG: &str = "-";
static LONG_FLAG: &str = "--";

#[derive(Debug, PartialEq, Eq)]
pub enum FlagErrorKind {
    IncorrectNumberOfDashes,
    UnrecognizedFlagName,
    IsAValue,
    UsageError,
    InvalidBooleanValue,
    MissingRequiredValue,
}

#[derive(Debug)]
pub struct FlagError {
    pub error_type: FlagErrorKind,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "error parsing flag value into specified type".fmt(f)
    }
}

pub struct Flag {
    shortname: Option<char>,
    longname: Option<String>,
    description: String,
    default_value: Option<Box<dyn Any>>,
    mandatory: bool,
    value_type: TypeId,
    value_unparsed: Option<String>,
}

impl Flag {
    pub fn new(
        shortname: Option<char>,
        longname: Option<String>,
        description: String,
        mandatory: bool,
        value_type: TypeId,
        default_value: Option<Box<dyn Any>>,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            shortname,
            longname,
            description,
            default_value,
            mandatory,
            value_type,
            value_unparsed: None,
        }))
    }

    pub fn type_id_of<T: 'static>() -> TypeId {
        TypeId::of::<T>()
    }

    pub fn shortname(&self) -> Option<&char> {
        self.shortname.as_ref()
    }

    pub fn longname(&self) -> Option<&String> {
        self.longname.as_ref()
    }

    pub fn description(&self) -> &String {
        &self.description
    }

    pub fn mandatory(&self) -> bool {
        self.mandatory
    }

    pub fn value_type(&self) -> TypeId {
        self.value_type
    }

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
                return Err("value and default value are not available".to_string());
            }
        }
    }

    fn get_default_value<T: Clone + 'static>(&self) -> Option<T> {
        let t = TypeId::of::<T>();
        if self.value_type != t {
            panic!("flag value type does not match {:?}", t);
        }
        let v = self
            .default_value
            .as_ref()
            .expect("cannot find default value");
        let t = v.downcast_ref::<T>();
        let t = t.expect("unexpected error unwrapping default value");
        let t = (*t).clone();
        Some(t)
    }

    fn get_value_unparsed(&self) -> Option<&String> {
        self.value_unparsed.as_ref()
    }

    fn set_value_unparsed(&mut self, s: Option<String>) {
        self.value_unparsed = s;
    }
}

pub struct FlagSet {
    flag_pair: HashMap<String, String>,
    flag_map: HashMap<String, Rc<RefCell<Flag>>>,
}

impl fmt::Display for Flag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut usage = String::new();

        if let (Some(shortname), Some(longname)) = (self.shortname(), self.longname()) {
            usage.push_str(&format!(
                "{}{} {}{}",
                SHORT_FLAG, shortname, LONG_FLAG, longname
            ));
        } else if let Some(shortname) = self.shortname() {
            usage.push_str(&format!("{}{}", SHORT_FLAG, shortname));
        } else if let Some(longname) = self.longname() {
            usage.push_str(&format!("{}{}", LONG_FLAG, longname));
        }
        usage.push_str(&format!(" {}", self.description()));
        if self.mandatory() {
            usage.push_str("(mandatory) ");
        }
        // TODO
        // if let Some(default_value) = self.get_default_value() {
        //     usage.push_str(&format!("(default: {})", default_value));
        // }
        f.write_str(&usage)
    }
}

impl FlagSet {
    pub fn new() -> Self {
        let flag_map = HashMap::new();
        let flag_pair = HashMap::new();
        Self {
            flag_pair,
            flag_map,
        }
    }

    pub fn add(&mut self, f: &Rc<RefCell<Flag>>) {
        let flag = f.borrow();
        let shortname = flag.shortname();
        let longname = flag.longname();

        if shortname == None && longname == None {
            panic!("required: short name or long name");
        }
        if let (Some(shortname), Some(longname)) = (shortname, longname) {
            self.flag_pair
                .insert(longname.to_string().clone(), shortname.to_string().clone());
            self.flag_pair
                .insert(shortname.to_string().clone(), longname.to_string().clone());
            self.flag_map
                .insert(shortname.to_string().clone(), Rc::clone(f));
            return;
        }
        let has_short_flag = shortname.is_some();
        let has_long_flag = longname.is_some();
        if has_short_flag {
            let shortname = shortname.expect("missing short flag name");
            self.flag_map
                .insert(shortname.to_string().clone(), Rc::clone(f));
            return;
        }
        if has_long_flag {
            let longname = longname.expect("missing long flag name");
            self.flag_map
                .insert(longname.to_string().clone(), Rc::clone(f));
        }
    }

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

    fn parse_args(&mut self, args: Vec<String>) -> Result<(), FlagError> {
        let mut args = args.iter().peekable();
        let mut flag_name = String::new();
        let mut is_value: bool = false;
        loop {
            if let Some(arg) = args.next() {
                if let Some(flag_error) = self.check_arg(arg).err() {
                    if flag_name.is_empty() {
                        return Err(FlagError {
                            error_type: FlagErrorKind::UsageError,
                            message: flag_error.message,
                        });
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
                    flag_name.push_str(arg.trim_start_matches("-"));
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
                        // TODO case where user passes true/false or yes/no.
                        if TypeId::of::<bool>() == value_type {
                            if arg.eq_ignore_ascii_case("true") || arg.eq_ignore_ascii_case("false")
                            {
                                self.set_flag_value_unparsed(&flag_name, arg.clone());
                            } else {
                                return Err(FlagError {
                                    error_type: FlagErrorKind::UsageError,
                                    message: format!("invalid boolean value"),
                                });
                            }
                        } else {
                            self.set_flag_value_unparsed(&flag_name, arg.clone());
                        }
                    } else {
                        // is not a value (arg is the next flag).
                        // check if
                        if TypeId::of::<bool>() == value_type {
                            self.set_flag_value_unparsed(&flag_name, "true".to_string());
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
        let flag_name = arg.trim_start_matches("-");
        let flag = self.flag_map.get(flag_name);
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
    use super::*;

    #[test]
    fn test_flag_with_value_ok() {
        let retry_flag = Flag::new(
            Some('r'),
            Some(String::from("retry")),
            String::from("number of retry operations"),
            false,
            Flag::type_id_of::<i32>(),
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
    fn test_optional_flag_with_default_value() {
        let retry_flag = Flag::new(
            Some('r'),
            Some(String::from("retry")),
            String::from("number of retry operations"),
            false,
            Flag::type_id_of::<i32>(),
            Some(Box::new(3i32)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(&retry_flag);
        let args = vec!["-r"];
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<String>>();
        match flagset.parse_args(args) {
            Ok(_) => {}
            Err(_) => assert!(true, "unexpected error parsing arguments"),
        }
        let v = retry_flag.borrow().get_value::<i32>();
        match v {
            Ok(v) => assert_eq!(v, 3),
            Err(e) => assert!(false, "{}", e),
        }
    }

    #[test]
    fn test_multiple_flags_with_err() {
        let bflag = Flag::new(
            Some('b'),
            Some("backup-path".to_string()),
            "path to the directory that can hold the backup files".to_string(),
            true,
            Flag::type_id_of::<String>(),
            Some(Box::new("/root/backup/10102022".to_string())),
        );
        let retry_flag = Flag::new(
            Some('r'),
            Some(String::from("retry")),
            String::from("number of retry operations"),
            false,
            Flag::type_id_of::<i32>(),
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
                assert_eq!(v.message, format!("missing required value for b"));
            }
            None => {
                assert!(
                    false,
                    "a non-boolean flag type should be provided with a value"
                );
            }
        }
    }

    // #[test]
    // fn test_flag_no_default_value() {
    //     let tflag = Flag::new(
    //         Some('i'),
    //         Some("ignore-case"),
    //         "case insensitive search",
    //         Some(false),
    //         false,
    //     );
    //     assert_eq!(tflag.shortname, Some('i'));
    //     assert_eq!(tflag.longname, Some("ignore-case"));
    //     assert_eq!(tflag.description, "case insensitive search");
    //     assert_eq!(tflag.default_value, Some(false));
    //     assert_eq!(tflag.mandatory, false);
    // }

    // #[test]
    // fn test_short_flag_added() {
    //     let mut flagset = FlagSet::new();
    //     let tflag = Flag::new(
    //         Some('b'),
    //         None,
    //         "path to the directory that can hold the backup files",
    //         Some("/root/backup/10102022".to_string()),
    //         true,
    //     );
    //     flagset.add(tflag);
    //     assert_eq!(flagset.is_known_flag("b"), true);
    // }

    //    #[test]
    //    fn test_add_multiple_flags() {
    //        // let bflag = Flag::new(
    //        //     Some('b'),
    //        //     None,
    //        //     "path to the directory that can hold the backup files",
    //        //     Some("/root/backup/10102022".to_string()),
    //        //     true,
    //        // );
    //        let iflag = Flag::new(
    //            Some('i'),
    //            Some("ignore-case"),
    //            "case insensitive search",
    //            Some(false),
    //            false,
    //        );
    //
    //        let mut flagset = FlagSet::new();
    //        flagset.add(iflag);
    //        assert_eq!(flagset.is_known_flag("ignore-case"), true);
    //    }

    //    #[test]
    //    #[should_panic(expected = "required: short name or long name")]
    //    fn test_with_no_flagnames() {
    //        let choice = FlagValue::Choice(false);
    //        let mut flagset = FlagSet::new();
    //        let iflag = Flag::new(None, None, "case insensitive", None, false, choice);
    //        flagset.add(iflag);
    //    }
    //
    //    #[test]
    //    fn test_formatter_both_flags() {
    //        let choice = FlagValue::Choice(false);
    //        let iflag = Flag::new(
    //            Some('i'),
    //            Some("case-insensitive"),
    //            "case insensitive",
    //            None,
    //            false,
    //            choice,
    //        );
    //        assert_eq!(
    //            format!("{}", iflag),
    //            "-i --case-insensitive case insensitive"
    //        )
    //    }
    //
    //    #[test]
    //    fn test_formatter_shortflag_only() {
    //        let choice = FlagValue::Choice(false);
    //        let iflag = Flag::new(Some('i'), None, "case insensitive", None, false, choice);
    //        assert_eq!(format!("{}", iflag), "-i case insensitive")
    //    }

    //    #[test]
    //    fn test_formatter_longflag_only() {
    //        let choice = FlagValue::Choice(false);
    //        let iflag = Flag::new(
    //            None,
    //            Some("case-insensitive"),
    //            "case insensitive",
    //            None,
    //            false,
    //            choice,
    //        );
    //        assert_eq!(format!("{}", iflag), "--case-insensitive case insensitive")
    //    }

    //    #[test]
    //    fn test_flag_default_value() {
    //        let a_number = FlagValue::I32(0);
    //        let retry_flag = Flag::new(
    //            Some('r'),
    //            Some("retry"),
    //            "number of times to retry",
    //            Some(FlagValue::I32(3)),
    //            false,
    //            a_number.clone(),
    //        );
    //        let timeout_flag = Flag::new(
    //            Some('t'),
    //            Some("timeout"),
    //            "timeout in seconds",
    //            Some(FlagValue::I32(10)),
    //            false,
    //            a_number.clone(),
    //        );
    //        let mut flagset = FlagSet::new();
    //        flagset.add(retry_flag);
    //        flagset.add(timeout_flag);
    //        match flagset.value_of("retry") {
    //            Ok(v) => match v {
    //                Some(flag_value) => {
    //                    assert_eq!(flag_value, &FlagValue::I32(3));
    //                }
    //                None => {
    //                    assert_eq!(true, false, "Flag value not found");
    //                }
    //            },
    //            Err(_) => {}
    //        }
    //    }
}
