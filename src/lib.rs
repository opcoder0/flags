use std::collections::HashMap;
use std::env;
use std::fmt;
use std::str::FromStr;
use std::string::ToString;

static SHORT_FLAG: &str = "-";
static LONG_FLAG: &str = "--";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "error parsing flag value into specified type".fmt(f)
    }
}

pub trait ValueType: fmt::Display + fmt::Debug {
    fn value_of(&mut self, s: &str);
}

impl<T> ValueType for T
where
    T: FromStr + Default + fmt::Display + ToString + fmt::Debug,
{
    fn value_of(&mut self, s: &str) {
        let r = Self::from_str(s);
        if r.is_ok() {
            *self = r.unwrap_or_default();
        }
    }
}

pub trait CommandlineArgument {
    fn shortname(&self) -> Option<&char>;
    fn longname(&self) -> Option<&String>;
    fn description(&self) -> &String;
    fn mandatory(&self) -> bool;
    fn get_value(&self) -> Option<&dyn ValueType>;
    fn set_value(&mut self, v: Box<dyn ValueType>);
    fn get_value_unparsed(&self) -> Option<&String>;
    fn set_value_unparsed(&mut self, s: Option<String>);
    fn get_default_value(&self) -> Option<&dyn ValueType>;
}

pub struct Flag {
    shortname: Option<char>,
    longname: Option<String>,
    description: String,
    default_value: Option<Box<dyn ValueType>>,
    mandatory: bool,
    value: Option<Box<dyn ValueType>>,
    value_unparsed: Option<String>,
}

impl Flag {
    pub fn new(
        shortname: Option<char>,
        longname: Option<String>,
        description: String,
        mandatory: bool,
        default_value: Option<Box<dyn ValueType>>,
    ) -> Self {
        Self {
            shortname,
            longname,
            description,
            default_value,
            mandatory,
            value: None,
            value_unparsed: None,
        }
    }
}

impl CommandlineArgument for Flag {
    fn shortname(&self) -> Option<&char> {
        self.shortname.as_ref()
    }

    fn longname(&self) -> Option<&String> {
        self.longname.as_ref()
    }

    fn description(&self) -> &String {
        &self.description
    }

    fn mandatory(&self) -> bool {
        self.mandatory
    }

    fn get_value(&self) -> Option<&dyn ValueType> {
        let v = self.value.as_ref();
        match v {
            Some(v) => Some(v.as_ref()),
            None => None,
        }
    }

    fn get_default_value(&self) -> Option<&dyn ValueType> {
        let v = self.default_value.as_ref();
        match v {
            Some(v) => Some(v.as_ref()),
            None => None,
        }
    }

    fn get_value_unparsed(&self) -> Option<&String> {
        self.value_unparsed.as_ref()
    }

    fn set_value(&mut self, v: Box<dyn ValueType>) {
        self.value = Some(v);
    }

    fn set_value_unparsed(&mut self, s: Option<String>) {
        self.value_unparsed = s;
    }
}

pub struct FlagSet {
    flag_pair: HashMap<String, String>,
    flag_map: HashMap<String, Box<dyn CommandlineArgument>>,
}

impl fmt::Display for dyn CommandlineArgument {
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
        if let Some(default_value) = self.get_default_value() {
            usage.push_str(&format!("(default: {})", default_value));
        }
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

    pub fn add(&mut self, f: Box<dyn CommandlineArgument>) {
        let shortname = f.shortname();
        let longname = f.longname();

        if shortname == None && longname == None {
            panic!("required: short name or long name");
        }
        if let (Some(shortname), Some(longname)) = (shortname, longname) {
            self.flag_pair
                .insert(longname.to_string().clone(), shortname.to_string().clone());
            self.flag_pair
                .insert(shortname.to_string().clone(), longname.to_string().clone());
            self.flag_map.insert(shortname.to_string().clone(), f);
            return;
        }
        let has_short_flag = shortname.is_some();
        let has_long_flag = longname.is_some();
        if has_short_flag {
            let shortname = shortname.expect("missing short flag name");
            self.flag_map.insert(shortname.to_string().clone(), f);
            return;
        }
        if has_long_flag {
            let longname = longname.expect("missing long flag name");
            self.flag_map.insert(longname.to_string().clone(), f);
        }
    }

    pub fn parse(&mut self, args: &mut env::Args) -> Result<(), String> {
        args.next(); // skip the program name.
        loop {
            if let Some(arg) = args.next() {
                if self.is_valid_prefix(&arg) {
                    let argname = arg.trim_start_matches("-");
                    if !self.is_known_flag(argname) {
                        return Err(format!("unknown flag name {}", arg));
                    }
                    // check if value follows
                    match args.next() {
                        Some(next_arg) => {
                            if self.num_dashes(&next_arg) == 0 {
                                let flag = self.flag_map.get_mut(argname);
                                let flag = flag.expect("unexpected error: flag not found");
                                flag.set_value_unparsed(Some(next_arg));
                            } else {
                                // next flag: check if the argname requires a value and if a default was
                                // provided.
                                let flag = self.flag_map.get_mut(argname);
                                let flag = flag.expect("unexpected error: flag not found");
                                match flag.get_default_value() {
                                    Some(default_value) => {
                                        let v = default_value.to_string();
                                        flag.set_value_unparsed(Some(v));
                                    }
                                    None => {
                                        return Err(format!(
                                            "missing required value for {}. Default value not set",
                                            argname
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            break;
                        }
                    }
                } else {
                    return Err("invalid flag format".to_string());
                }
            } else {
                break;
            }
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

    fn is_known_flag(&self, k: &str) -> bool {
        self.flag_map.contains_key(k)
    }
}

// fn value_of<F>(s: &str) -> Result<F, <F as FromStr>::Err>
// where
//     F: FromStr + Clone,
// {
//     s.parse::<F>()
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_flag() {
        let retry_flag = Flag::new(
            Some('r'),
            Some(String::from("retry")),
            String::from("number of retry operations"),
            false,
            Some(Box::new(3i32)),
        );
        let mut flagset = FlagSet::new();
        flagset.add(Box::new(retry_flag));
        assert_eq!(retry_flag.shortname, Some('r'));
        assert_eq!(retry_flag.longname, Some(String::from("retry")));
        assert_eq!(
            retry_flag.description,
            String::from("number of retry operations")
        );
        assert_eq!(retry_flag.default_value, Some(Box::new(3i32)));
        assert_eq!(retry_flag.mandatory, false);
    }

    // #[test]
    // fn test_flag_mandatory() {
    //     let tflag = Flag::new(
    //         Some('b'),
    //         Some("backup-path"),
    //         "path to the directory that can hold the backup files",
    //         Some("/root/backup/10102022".to_string()),
    //         true,
    //     );
    //     assert_eq!(tflag.shortname, Some('b'));
    //     assert_eq!(tflag.longname, Some("backup-path"));
    //     assert_eq!(
    //         tflag.description,
    //         "path to the directory that can hold the backup files"
    //     );
    //     assert_eq!(
    //         tflag.default_value,
    //         Some("/root/backup/10102022".to_string())
    //     );
    //     assert_eq!(tflag.mandatory, true);
    // }

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
