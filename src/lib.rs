#![feature(concat_bytes)]
#![deny(absolute_paths_not_starting_with_crate)]
#![deny(keyword_idents)]
#![deny(explicit_outlives_requirements)]
#![deny(elided_lifetimes_in_paths)]
#![deny(macro_use_extern_crate)]
#![deny(meta_variable_misuse)]
#![deny(missing_abi)]
// #![deny(missing_copy_implementations)]
// #![deny(missing_debug_implementations)]
// #![deny(missing_docs)]
#![deny(non_ascii_idents)]
#![deny(noop_method_call)]
#![deny(pointer_structural_match)]
#![deny(semicolon_in_expressions_from_macros)]
#![deny(single_use_lifetimes)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unreachable_pub)]
// #![deny(unsafe_code)]
// #![deny(unsafe_op_in_unsafe_fn)]
#![deny(unused_extern_crates)]
#![deny(unused_import_braces)]
#![deny(unused_lifetimes)]
#![deny(unused_qualifications)]
#![deny(unused_results)]
#![deny(clippy::all)]
#![deny(clippy::cargo)]
#![deny(clippy::pedantic)]
#![allow(clippy::unit_arg)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::type_complexity)]
#![allow(clippy::multiple_crate_versions)]
#![allow(clippy::match_same_arms)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::new_without_default)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::useless_conversion)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::trivially_copy_pass_by_ref)]
#![allow(clippy::identity_op)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::needless_pass_by_value)]
#![allow(clippy::default_trait_access)]
#![allow(clippy::wildcard_dependencies)]
#![allow(clippy::cargo_common_metadata)]
#![allow(clippy::similar_names)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::unreadable_literal)]
#![allow(clippy::mixed_case_hex_literals)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::nonminimal_bool)]

pub mod apu;
pub mod bus;
pub mod cartridge;
pub mod cpu;
pub mod instruction;
pub mod machine;
pub mod rom;
pub mod serial;

pub mod mem_layout;
pub mod ppu;
pub use mem_layout::*;
