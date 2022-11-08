# Flags

Small and easy to use command line argument parser library for Rust.

## API

The API consists of 4 main types -

- `Flag` represents a commandline argument and its value.
- `FlagSet` represents a set of flags (typically used by the application).
- `FlagError` represents the error returned by `parse` (if any).
- `FlagErrorKind` represents the error code in  `FlagError`.

The API allows user to define flag with any types using the same APIs. Sample -

```
extern crate flags;

use flags::{Flag, FlagSet};
use std::env;
use std::process;

fn main() {
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
    let result = flagset.parse(&mut env::args());
    match result {
        Err(e) => {
            println!("{}", e);	// prints the error
            println!("{}", flagset); // prints the usage
            process::exit(1);
        }
        Ok(()) => {}
    }
    let backup_path = bflag.borrow().get_value::<String>().ok().unwrap(); // extracts value for the mandatory flag
    let num_retries = retry_flag.borrow().get_value::<i32>().ok().unwrap(); // extracts value or default value for optional flag
    println!("backup path: {}", backup_path);
    println!("number of retries: {}", num_retries);
}
```
