use std::collections::HashMap;
use std::env;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum FlagValue {
    // TODO use num trait to gather numeric types under a single value
    I32(i32),
    U32(u32),
    Text(String),
    Choice(bool),
}

pub struct Flag<'a> {
    shortname: Option<char>,
    longname: Option<&'a str>,
    description: &'a str,
    default_value: Option<FlagValue>,
    mandatory: bool,
    value_type: FlagValue,
    value: Option<FlagValue>,
}

pub struct FlagSet<'a> {
    flag_pair: HashMap<String, String>,
    flag_map: HashMap<String, Flag<'a>>,
}

impl<'a> Flag<'a> {
    pub fn new(
        shortname: Option<char>,
        longname: Option<&'a str>,
        description: &'a str,
        default_value: Option<FlagValue>,
        mandatory: bool,
        value_type: FlagValue,
    ) -> Self {
        Self {
            shortname,
            longname,
            description,
            default_value,
            mandatory,
            value_type,
            value: None,
        }
    }
}

impl<'a> fmt::Display for FlagValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I32(n) => f.write_str(&format!("{}", n)),
            Self::U32(n) => f.write_str(&format!("{}", n)),
            Self::Text(s) => f.write_str(s),
            Self::Choice(c) => f.write_str(&format!("{}", c)),
        }
    }
}

impl<'a> fmt::Display for Flag<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut usage = String::new();

        if let (Some(shortname), Some(longname)) = (self.shortname, self.longname) {
            usage.push_str(&format!("-{} --{}", shortname, longname));
        } else if let Some(shortname) = self.shortname {
            usage.push_str(&format!("-{}", shortname));
        } else if let Some(longname) = self.longname {
            usage.push_str(&format!("--{}", longname));
        }
        usage.push_str(&format!(" {}", self.description));
        if self.mandatory {
            usage.push_str("(mandatory) ");
        }
        if let Some(default_value) = &self.default_value {
            usage.push_str(&format!("(default: {})", default_value));
        }
        f.write_str(&usage)
    }
}

impl<'a> FlagSet<'a> {
    pub fn new() -> Self {
        let flag_map = HashMap::new();
        let flag_pair = HashMap::new();
        Self {
            flag_pair,
            flag_map,
        }
    }

    pub fn add(&mut self, f: Flag<'a>) {
        if f.shortname == None && f.longname == None {
            panic!("required: short name or long name");
        }
        if let (Some(shortname), Some(longname)) = (&f.shortname, &f.longname) {
            self.flag_pair
                .insert(longname.to_string().clone(), shortname.to_string().clone());
            self.flag_pair
                .insert(shortname.to_string().clone(), longname.to_string().clone());
            self.flag_map.insert(shortname.to_string().clone(), f);
            return;
        }
        let has_short_flag = f.shortname.is_some();
        let has_long_flag = f.longname.is_some();
        if has_short_flag {
            let shortname = f.shortname.expect("missing short flag name");
            self.flag_map.insert(shortname.to_string().clone(), f);
            return;
        }
        if has_long_flag {
            let longname = f.longname.expect("missing long flag name");
            self.flag_map.insert(longname.to_string().clone(), f);
        }
    }

    pub fn value_of(&self, argname: &'a str) -> Result<&Option<FlagValue>, String> {
        if !self.is_valid_flag(argname) {
            return Err("invalid flag name".to_string());
        }
        let f = self.flag_map.get(argname);
        match f {
            Some(flag) => {
                return Ok(&flag.value);
            }
            None => {
                return Err("value not set".to_string());
            }
        }
    }

    pub fn parse(&mut self, args: &mut env::Args) -> Result<(), String> {
        args.next(); // skip the program name.
        loop {
            if let Some(arg) = args.next() {
                if arg.starts_with("-") {
                    let argname = arg.trim_start_matches("-");
                    if !self.is_valid_flag(argname) {
                        return Err(format!("invalid flag name {}", arg));
                    }
                    let flag = self.flag_map.get_mut(argname);
                    match flag {
                        Some(f) => {
                            let argval = args.next().unwrap_or("".to_string());
                            match Self::set_flag_value(argval, f) {
                                Ok(_) => {}
                                Err(e) => {
                                    return Err(e);
                                }
                            }
                        }
                        None => {
                            return Err(format!("flag not found"));
                        }
                    }
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn set_flag_value(argval: String, flag: &mut Flag) -> Result<(), String> {
        match argval.len() {
            0 => {
                return Err("missing expected value".to_string());
            }
            _ => {
                if argval.starts_with("-") {
                    // looks like the next flag.
                    // use the default value if available.
                    if flag.default_value.is_some() {
                        flag.value = flag.default_value.clone();
                        return Ok(());
                    } else {
                        return Err("missing expected value".to_string());
                    }
                }
                match flag.value_type {
                    FlagValue::I32(_) => match argval.parse::<i32>() {
                        Ok(ival) => {
                            flag.value = Some(FlagValue::I32(ival));
                            return Ok(());
                        }
                        Err(_) => {
                            return Err(format!(
                                "invalid value {} expecting a signed integer type",
                                argval
                            ));
                        }
                    },
                    FlagValue::U32(_) => match argval.parse::<u32>() {
                        Ok(uval) => {
                            flag.value = Some(FlagValue::U32(uval));
                            return Ok(());
                        }
                        Err(_) => {
                            return Err(format!(
                                "invalid value {} expecting an unsigned integer type",
                                argval
                            ));
                        }
                    },
                    FlagValue::Text(_) => {
                        flag.value = Some(FlagValue::Text(argval.to_owned()));
                        return Ok(());
                    }
                    FlagValue::Choice(_) => {
                        flag.value = Some(FlagValue::Choice(true));
                        return Ok(());
                    }
                }
            }
        }
    }

    fn is_valid_flag(&self, k: &str) -> bool {
        self.flag_map.contains_key(k)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_flag() {
        let number = FlagValue::I32(0);
        let tflag = Flag::new(
            Some('i'),
            Some("int"),
            "integer flag",
            Some(FlagValue::I32(5)),
            false,
            number,
        );
        assert_eq!(tflag.shortname, Some('i'));
        assert_eq!(tflag.longname, Some("int"));
        assert_eq!(tflag.description, "integer flag");
        assert_eq!(tflag.default_value, Some(FlagValue::I32(5)));
        assert_eq!(tflag.mandatory, false);
    }

    #[test]
    fn test_flag_mandatory() {
        let text = FlagValue::Text("".to_string());
        let tflag = Flag::new(
            Some('s'),
            Some("str"),
            "string flag",
            Some(FlagValue::Text("default string".to_string())),
            true,
            text,
        );
        assert_eq!(tflag.shortname, Some('s'));
        assert_eq!(tflag.longname, Some("str"));
        assert_eq!(tflag.description, "string flag");
        assert_eq!(
            tflag.default_value,
            Some(FlagValue::Text("default string".to_string()))
        );
        assert_eq!(tflag.mandatory, true);
    }

    #[test]
    fn test_flag_no_default_value() {
        let text = FlagValue::Text("".to_string());
        let tflag = Flag::new(Some('s'), Some("str"), "string flag", None, false, text);
        assert_eq!(tflag.shortname, Some('s'));
        assert_eq!(tflag.longname, Some("str"));
        assert_eq!(tflag.description, "string flag");
        assert_eq!(tflag.default_value, None);
        assert_eq!(tflag.mandatory, false);
    }

    #[test]
    fn test_short_flag_added() {
        let text = FlagValue::Text("".to_string());
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(Some('i'), None, "case-sensitive", None, false, text);
        flagset.add(iflag);
        assert_eq!(flagset.is_valid_flag("i"), true);
    }

    #[test]
    fn test_long_flag_added() {
        let number = FlagValue::U32(0);
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(
            None,
            Some("ignore-case"),
            "case insensitive",
            None,
            false,
            number,
        );
        flagset.add(iflag);
        assert_eq!(flagset.is_valid_flag("ignore-case"), true);
    }

    #[test]
    #[should_panic(expected = "required: short name or long name")]
    fn test_with_no_flagnames() {
        let choice = FlagValue::Choice(false);
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(None, None, "case insensitive", None, false, choice);
        flagset.add(iflag);
    }

    #[test]
    fn test_formatter_both_flags() {
        let choice = FlagValue::Choice(false);
        let iflag = Flag::new(
            Some('i'),
            Some("case-insensitive"),
            "case insensitive",
            None,
            false,
            choice,
        );
        assert_eq!(
            format!("{}", iflag),
            "-i --case-insensitive case insensitive"
        )
    }

    #[test]
    fn test_formatter_shortflag_only() {
        let choice = FlagValue::Choice(false);
        let iflag = Flag::new(Some('i'), None, "case insensitive", None, false, choice);
        assert_eq!(format!("{}", iflag), "-i case insensitive")
    }

    #[test]
    fn test_formatter_longflag_only() {
        let choice = FlagValue::Choice(false);
        let iflag = Flag::new(
            None,
            Some("case-insensitive"),
            "case insensitive",
            None,
            false,
            choice,
        );
        assert_eq!(format!("{}", iflag), "--case-insensitive case insensitive")
    }

    #[test]
    fn test_flag_default_value() {
        let a_number = FlagValue::I32(0);
        let retry_flag = Flag::new(
            Some('r'),
            Some("retry"),
            "number of times to retry",
            Some(FlagValue::I32(3)),
            false,
            a_number.clone(),
        );
        let timeout_flag = Flag::new(
            Some('t'),
            Some("timeout"),
            "timeout in seconds",
            Some(FlagValue::I32(10)),
            false,
            a_number.clone(),
        );
        let mut flagset = FlagSet::new();
        flagset.add(retry_flag);
        flagset.add(timeout_flag);
        match flagset.value_of("retry") {
            Ok(v) => match v {
                Some(flag_value) => {
                    assert_eq!(flag_value, &FlagValue::I32(3));
                }
                None => {
                    assert_eq!(true, false, "Flag value not found");
                }
            },
            Err(_) => {}
        }
    }
}
