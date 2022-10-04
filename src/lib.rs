use std::collections::HashMap;
use std::env;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum FlagValue<'a> {
    // TODO use num trait to gather numeric types under a single value
    I32(i32),
    U32(u32),
    Text(&'a str),
    Choice(bool),
}

pub struct Flag<'a> {
    shortname: Option<char>,
    longname: Option<&'a str>,
    description: &'a str,
    default_value: Option<&'a FlagValue<'a>>,
    mandatory: bool,
    value: &'a mut FlagValue<'a>,
}

pub struct FlagSet<'a> {
    short_map: HashMap<char, &'a Flag<'a>>,
    long_map: HashMap<&'a str, &'a Flag<'a>>,
}

impl<'a> Flag<'a> {
    pub fn new(
        shortname: Option<char>,
        longname: Option<&'a str>,
        description: &'a str,
        default_value: Option<&'a FlagValue<'a>>,
        mandatory: bool,
        value: &'a mut FlagValue<'a>,
    ) -> Self {
        Self {
            shortname,
            longname,
            description,
            default_value,
            mandatory,
            value,
        }
    }
}

impl<'a> fmt::Display for FlagValue<'a> {
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
        if let Some(default_value) = self.default_value {
            usage.push_str(&format!("(default: {})", default_value));
        }
        f.write_str(&usage)
    }
}

impl<'a> FlagSet<'a> {
    pub fn new() -> Self {
        let short_map = HashMap::new();
        let long_map = HashMap::new();
        Self {
            short_map,
            long_map,
        }
    }

    pub fn add(&mut self, f: &'a mut Flag<'a>) {
        if f.shortname == None && f.longname == None {
            panic!("required: short name or long name");
        }
        if let Some(longname) = f.longname {
            self.long_map.insert(longname, f);
        }
        if let Some(shortname) = f.shortname {
            self.short_map.insert(shortname, f);
        }
    }

    pub fn parse(&mut self, args: &mut env::Args) -> Result<(), String> {
        let arguments: Vec<String> = args.collect();
        let mut iter = arguments.iter();
        loop {
            if let Some(arg) = iter.next() {
                if arg.starts_with("-") {
                    let argname = arg.trim_start_matches("-");
                    if !self.is_valid_flag(argname) {
                        return Err(format!("invalid flag name {}", arg));
                    }
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn set_flag(&mut self, flag_name: &str, args: &mut env::Args) {}

    fn get_flag(&self, k: &str) -> Option<&&Flag<'a>> {
        match k.len() {
            0 => None,
            1 => {
                let c = k.chars().next().expect("unexpected flag length");
                self.short_map.get(&c)
            }
            _ => self.long_map.get(k),
        }
    }

    fn is_valid_flag(&self, k: &str) -> bool {
        match k.len() {
            0 => false,
            1 => match k.chars().next() {
                Some(c) => self.short_map.contains_key(&c),
                None => false,
            },
            _ => self.long_map.contains_key(k),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_flag() {
        let mut value = FlagValue::I32(0);
        let tflag = Flag::new(
            Some('i'),
            Some("int"),
            "integer flag",
            Some(&FlagValue::I32(5)),
            false,
            &mut value,
        );
        assert_eq!(tflag.shortname, Some('i'));
        assert_eq!(tflag.longname, Some("int"));
        assert_eq!(tflag.description, "integer flag");
        assert_eq!(tflag.default_value, Some(&FlagValue::I32(5)));
        assert_eq!(tflag.mandatory, false);
    }

    #[test]
    fn test_flag_mandatory() {
        let mut value = FlagValue::Text("");
        let tflag = Flag::new(
            Some('s'),
            Some("str"),
            "string flag",
            Some(&FlagValue::Text("default string")),
            true,
            &mut value,
        );
        assert_eq!(tflag.shortname, Some('s'));
        assert_eq!(tflag.longname, Some("str"));
        assert_eq!(tflag.description, "string flag");
        assert_eq!(
            tflag.default_value,
            Some(&FlagValue::Text("default string"))
        );
        assert_eq!(tflag.mandatory, true);
    }

    #[test]
    fn test_flag_no_default_value() {
        let mut value = FlagValue::Text("");
        let tflag = Flag::new(
            Some('s'),
            Some("str"),
            "string flag",
            None,
            false,
            &mut value,
        );
        assert_eq!(tflag.shortname, Some('s'));
        assert_eq!(tflag.longname, Some("str"));
        assert_eq!(tflag.description, "string flag");
        assert_eq!(tflag.default_value, None);
        assert_eq!(tflag.mandatory, false);
    }

    #[test]
    fn test_short_flag_added() {
        let mut value = FlagValue::Text("");
        let mut flagset = FlagSet::new();
        let mut iflag = Flag::new(Some('i'), None, "case-sensitive", None, false, &mut value);
        flagset.add(&mut iflag);
        assert_eq!(flagset.is_valid_flag("i"), true);
    }

    #[test]
    fn test_long_flag_added() {
        let mut value = FlagValue::U32(0);
        let mut flagset = FlagSet::new();
        let mut iflag = Flag::new(
            None,
            Some("ignore-case"),
            "case insensitive",
            None,
            false,
            &mut value,
        );
        flagset.add(&mut iflag);
        assert_eq!(flagset.is_valid_flag("ignore-case"), true);
    }

    #[test]
    #[should_panic(expected = "required: short name or long name")]
    fn test_with_no_flagnames() {
        let mut value = FlagValue::Choice(false);
        let mut flagset = FlagSet::new();
        let mut iflag = Flag::new(None, None, "case insensitive", None, false, &mut value);
        flagset.add(&mut iflag);
    }

    #[test]
    fn test_formatter_both_flags() {
        let mut value = FlagValue::Choice(false);
        let iflag = Flag::new(
            Some('i'),
            Some("case-insensitive"),
            "case insensitive",
            None,
            false,
            &mut value,
        );
        assert_eq!(
            format!("{}", iflag),
            "-i --case-insensitive case insensitive"
        )
    }

    #[test]
    fn test_formatter_shortflag_only() {
        let mut value = FlagValue::Choice(false);
        let iflag = Flag::new(Some('i'), None, "case insensitive", None, false, &mut value);
        assert_eq!(format!("{}", iflag), "-i case insensitive")
    }

    #[test]
    fn test_formatter_longflag_only() {
        let mut value = FlagValue::Choice(false);
        let iflag = Flag::new(
            None,
            Some("case-insensitive"),
            "case insensitive",
            None,
            false,
            &mut value,
        );
        assert_eq!(format!("{}", iflag), "--case-insensitive case insensitive")
    }
}
