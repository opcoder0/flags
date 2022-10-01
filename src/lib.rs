use std::collections::HashMap;
use std::fmt;

pub struct Flag<'a> {
    shortname: Option<char>,
    longname: Option<&'a str>,
    description: &'a str,
    default_value: Option<&'a str>,
    mandatory: bool,
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
        default_value: Option<&'a str>,
        mandatory: bool,
    ) -> Self {
        Self {
            shortname,
            longname,
            description,
            default_value,
            mandatory,
        }
    }
}

impl<'a> fmt::Display for Flag<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut usage = String::new();
        if let Some(shortname) = self.shortname {
            let fmt_str = format!("-{}", shortname);
            usage.push_str(&fmt_str);
        }
        if let Some(longname) = self.longname {
            let fmt_str = format!(" --{}", longname);
            usage.push_str(&fmt_str);
        }
        let fmt_str = format!(" {}", self.description);
        usage.push_str(&fmt_str);
        if self.mandatory {
            usage.push_str("(mandatory)");
        }
        if let Some(default_value) = self.default_value {
            let fmt_str = format!(" (default: {})", default_value);
            usage.push_str(&fmt_str);
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

    pub fn add(&mut self, f: &'a Flag) {
        if f.shortname == None && f.longname == None {
            panic!("required: short name or long name");
        }
        if let Some(longname) = f.longname {
            self.long_map.insert(longname, f);
        } else {
            if let Some(shortname) = f.shortname {
                self.short_map.insert(shortname, f);
            }
        }
    }

    fn _shortnames(&self) -> &HashMap<char, &'a Flag> {
        &self.short_map
    }

    fn _longnames(&self) -> &HashMap<&'a str, &'a Flag> {
        &&self.long_map
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_flag() {
        let tflag = Flag::new(Some('i'), Some("int"), "integer flag", Some("5"), false);
        assert_eq!(tflag.shortname, Some('i'));
        assert_eq!(tflag.longname, Some("int"));
        assert_eq!(tflag.description, "integer flag");
        assert_eq!(tflag.default_value, Some("5"));
        assert_eq!(tflag.mandatory, false);
    }

    #[test]
    fn test_flag_mandatory() {
        let tflag = Flag::new(
            Some('s'),
            Some("str"),
            "string flag",
            Some("default string"),
            true,
        );
        assert_eq!(tflag.shortname, Some('s'));
        assert_eq!(tflag.longname, Some("str"));
        assert_eq!(tflag.description, "string flag");
        assert_eq!(tflag.default_value, Some("default string"));
        assert_eq!(tflag.mandatory, true);
    }

    #[test]
    fn test_flag_no_default_value() {
        let tflag = Flag::new(Some('s'), Some("str"), "string flag", None, false);
        assert_eq!(tflag.shortname, Some('s'));
        assert_eq!(tflag.longname, Some("str"));
        assert_eq!(tflag.description, "string flag");
        assert_eq!(tflag.default_value, None);
        assert_eq!(tflag.mandatory, false);
    }

    #[test]
    fn test_short_flag_added() {
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(Some('i'), None, "case-sensitive", None, false);
        flagset.add(&iflag);
        let smap = flagset._shortnames();
        assert_eq!(smap.contains_key(&'i'), true);
    }

    #[test]
    fn test_long_flag_added() {
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(None, Some("ignore-case"), "case insensitive", None, false);
        flagset.add(&iflag);
        let lmap = flagset._longnames();
        assert_eq!(lmap.contains_key("ignore-case"), true);
    }

    #[test]
    #[should_panic(expected = "required: short name or long name")]
    fn test_with_no_flagnames() {
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(None, None, "case insensitive", None, false);
        flagset.add(&iflag);
    }

    #[test]
    fn test_formatter_both_flags() {
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(
            Some('i'),
            Some("case-insensitive"),
            "case insensitive",
            None,
            false,
        );
        flagset.add(&iflag);
        assert_eq!(
            format!("{}", iflag),
            "-i --case-insensitive case insensitive"
        )
    }

    #[test]
    fn test_formatter_shortflag_only() {
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(Some('i'), None, "case insensitive", None, false);
        flagset.add(&iflag);
        assert_eq!(format!("{}", iflag), "-i case insensitive")
    }

    #[test]
    fn test_formatter_longflag_only() {
        let mut flagset = FlagSet::new();
        let iflag = Flag::new(
            None,
            Some("case-insensitive"),
            "case insensitive",
            None,
            false,
        );
        flagset.add(&iflag);
        assert_eq!(format!("{}", iflag), "--case-insensitive case insensitive")
    }
}
