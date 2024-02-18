#![allow(clippy::from_over_into)]

use std::fmt::{Debug, Formatter};

use anathema_render::Color;

pub use self::num::Num;
pub use self::owned::Static;
use crate::hashmap::HashMap;
use crate::{Collection, Expression, List, Map, State};

mod num;
mod owned;
