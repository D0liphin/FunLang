use super::LlvmType;

pub const TYPENAME_PINT: &'static str = "Int";
pub const TYPENAME_PDOUBLE: &'static str = "Double";
pub const TYPENAME_PSTRING: &'static str = "String";
pub const TYPENAME_PBOOL: &'static str = "Bool";
pub const TYPENAME_PVOID: &'static str = "Void";

pub const BUILTIN_IADD: &'static str = "builtin.iadd";
pub const BUILTIN_ISUB: &'static str = "builtin.isub";
pub const BUILTIN_IMUL: &'static str = "builtin.imul";
pub const BUILTIN_IDIV: &'static str = "builtin.idiv";
pub const BUILTIN_IREM: &'static str = "builtin.irem";

pub const BUILTIN_FADD: &'static str = "builtin.fadd";
pub const BUILTIN_FSUB: &'static str = "builtin.fsub";
pub const BUILTIN_FMUL: &'static str = "builtin.fmul";
pub const BUILTIN_FDIV: &'static str = "builtin.fdiv";

pub const BUILTIN_ICMP_EQ: &'static str = "builtin.icmp.eq";
pub const BUILTIN_ICMP_NE: &'static str = "builtin.icmp.ne";
pub const BUILTIN_ICMP_SGT: &'static str = "builtin.icmp.sgt";
pub const BUILTIN_ICMP_SGE: &'static str = "builtin.icmp.sge";
pub const BUILTIN_ICMP_SLT: &'static str = "builtin.icmp.slt";
pub const BUILTIN_ICMP_SLE: &'static str = "builtin.icmp.sle";

pub const BUILTIN_FCMP_EQ: &'static str = "builtin.fcmp.eq";
pub const BUILTIN_FCMP_NE: &'static str = "builtin.fcmp.ne";
pub const BUILTIN_FCMP_UGT: &'static str = "builtin.fcmp.ugt";
pub const BUILTIN_FCMP_UGE: &'static str = "builtin.fcmp.uge";
pub const BUILTIN_FCMP_ULT: &'static str = "builtin.fcmp.ult";
pub const BUILTIN_FCMP_ULE: &'static str = "builtin.fcmp.ule";

pub const BUILTIN_IWRITE: &'static str = "builtin.iwrite";
pub const BUILTIN_FWRITE: &'static str = "builtin.dwrite";
pub const BUILTIN_SWRITE: &'static str = "builtin.swrite";

pub const LLVM_ISIZE: LlvmType = LlvmType::Int(POINTER_WIDTH);
pub const LLVM_BOOL: LlvmType = LlvmType::Int(1);

pub fn llvm_pstatic_string() -> LlvmType {
    LlvmType::LiteralStructure(vec![LlvmType::Ptr, LLVM_ISIZE])
}

#[cfg(target_pointer_width = "64")]
pub const POINTER_WIDTH: u32 = 64;

#[cfg(target_pointer_width = "32")]
pub const POINTER_WIDTH: u32 = 32;

#[cfg(target_pointer_width = "16")]
pub const POINTER_WIDTH: u32 = 16;
