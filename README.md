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

More complex types can be passed in as flags too as long as they implement `FromStr` and `Clone` traits. Example -

```
extern crate flags;

use flags::{Flag, FlagSet};
use std::env;
use std::num::ParseIntError;
use std::process;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Clone for Point {
    fn clone(&self) -> Self {
        let x = self.x;
        let y = self.y;
        Self { x, y }
    }
}

impl ToString for Point {
    fn to_string(&self) -> String {
        format!("({},{})", self.x, self.y)
    }
}

impl FromStr for Point {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .and_then(|s| s.split_once(','))
            .unwrap();

        let x_fromstr = x.parse::<i32>()?;
        let y_fromstr = y.parse::<i32>()?;

        Ok(Point {
            x: x_fromstr,
            y: y_fromstr,
        })
    }
}

fn main() {
    let point_flag = Flag::new(
        Some("-p"),
        Some("--point"),
        "point on the 2d plane",
        true,
        Flag::kind::<Point>(),
        Some(Box::new(Point { x: 0, y: 0 })),
    );

    let mut flagset = FlagSet::new();
    flagset.add(&point_flag);
    let result = flagset.parse(&mut env::args());
    match result {
        Err(e) => {
            println!("{}", e);
            println!("{}", flagset);
            process::exit(1);
        }
        Ok(()) => {}
    }
    let coord = point_flag.borrow().get_value::<Point>().ok().unwrap();
    println!("co-ordinates: {:?}", coord);
}
```
