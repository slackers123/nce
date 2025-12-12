#[cfg(target_arch = "aarch64")]
pub use crate::arch::aarch64::codegen::*;

#[cfg(target_arch = "x86_64")]
pub use crate::arch::x86_64::codegen::*;
