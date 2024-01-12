; This file was generated by clang and then modified by me (Oli) to remove some
; debug symbols and make sure it is definitely compatible with the IR I
; generate for .fun files 
;
; Lots of the intrinsics are hand-written LLVM
; 
; This file only actually works on 64-bit systems, though endianness is not a
; concern
@stdout = external local_unnamed_addr global ptr, align 8
@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"%g\00", align 1

; Extern libc declarations 
declare noundef i64 @fwrite(ptr nocapture noundef, i64 noundef, i64 noundef, ptr nocapture noundef) local_unnamed_addr
declare noundef i32 @fflush(ptr nocapture noundef) local_unnamed_addr
declare noundef i32 @printf(ptr nocapture noundef readonly, ...) local_unnamed_addr

; funstd and friends
define dso_local void @builtin.swrite({ ptr, i64 } %string) local_unnamed_addr {
    %1 = extractvalue { ptr, i64 } %string, 0
    %2 = extractvalue { ptr, i64 } %string, 1
    %3 = load ptr, ptr @stdout, align 8
    %4 = tail call i64 @fwrite(ptr noundef %1, i64 noundef 1, i64 noundef %2, ptr noundef %3)
    %5 = load ptr, ptr @stdout, align 8
    %6 = tail call i32 @fflush(ptr noundef %5)
    ret void
}

define dso_local i64 @encode_utf8(ptr nocapture noundef writeonly %0, i32 noundef %1) local_unnamed_addr {
    %3 = icmp ult i32 %1, 128
    br i1 %3, label %4, label %6

4:
    %5 = trunc i32 %1 to i8
    store i8 %5, ptr %0, align 1
    br label %48

6:
    %7 = icmp ult i32 %1, 2048
    br i1 %7, label %8, label %16

8:
    %9 = lshr i32 %1, 6
    %10 = trunc i32 %9 to i8
    %11 = or i8 %10, -64
    store i8 %11, ptr %0, align 1
    %12 = trunc i32 %1 to i8
    %13 = and i8 %12, 63
    %14 = or i8 %13, -128
    %15 = getelementptr inbounds i8, ptr %0, i64 1
    store i8 %14, ptr %15, align 1
    br label %48

16:
    %17 = icmp ult i32 %1, 65536
    %18 = getelementptr inbounds i8, ptr %0, i64 1
    br i1 %17, label %19, label %31

19:
    %20 = lshr i32 %1, 12
    %21 = trunc i32 %20 to i8
    %22 = or i8 %21, -32
    store i8 %22, ptr %0, align 1
    %23 = lshr i32 %1, 6
    %24 = trunc i32 %23 to i8
    %25 = and i8 %24, 63
    %26 = or i8 %25, -128
    store i8 %26, ptr %18, align 1
    %27 = trunc i32 %1 to i8
    %28 = and i8 %27, 63
    %29 = or i8 %28, -128
    %30 = getelementptr inbounds i8, ptr %0, i64 2
    store i8 %29, ptr %30, align 1
    br label %48

31:
    %32 = lshr i32 %1, 18
    %33 = trunc i32 %32 to i8
    %34 = or i8 %33, -16
    store i8 %34, ptr %0, align 1
    %35 = lshr i32 %1, 12
    %36 = trunc i32 %35 to i8
    %37 = and i8 %36, 63
    %38 = or i8 %37, -128
    store i8 %38, ptr %18, align 1
    %39 = lshr i32 %1, 6
    %40 = trunc i32 %39 to i8
    %41 = and i8 %40, 63
    %42 = or i8 %41, -128
    %43 = getelementptr inbounds i8, ptr %0, i64 2
    store i8 %42, ptr %43, align 1
    %44 = trunc i32 %1 to i8
    %45 = and i8 %44, 63
    %46 = or i8 %45, -128
    %47 = getelementptr inbounds i8, ptr %0, i64 3
    store i8 %46, ptr %47, align 1
    br label %48

48:
    %49 = phi i64 [ 1, %4 ], [ 2, %8 ], [ 3, %19 ], [ 4, %31 ]
    ret i64 %49
}

define dso_local zeroext i1 @builtin.valid_codepoint(i32 noundef %0) local_unnamed_addr {
    %2 = icmp slt i32 %0, 1114112
    %3 = and i32 %0, -2048
    %4 = icmp ne i32 %3, 55296
    %5 = and i1 %2, %4
    ret i1 %5
}

define dso_local void @builtin.ciwrite(i32 noundef %0) local_unnamed_addr {
    %2 = alloca [4 x i8], align 1
    %3 = icmp slt i32 %0, 1114112
    %4 = and i32 %0, -2048
    %5 = icmp ne i32 %4, 55296
    %6 = and i1 %3, %5
    %7 = select i1 %6, i32 %0, i32 65533
    %8 = call i64 @encode_utf8(ptr noundef nonnull %2, i32 noundef %7)
    ; make sure this is the right type -- not sure if this is actually 
    ; necessary to avoid UB, but I wrote it now!
    %string.alloc = alloca { ptr, i64 }
    %string.undef.0 = load { ptr, i64 }, ptr %string.alloc
    %string.undef.1 = insertvalue { ptr, i64 } %string.undef.0, ptr %2, 0
    %string = insertvalue { ptr, i64 } %string.undef.1, i64 %8, 1
    call void @builtin.swrite({ ptr, i64 } %string)
    ret void
}

define dso_local void @builtin.iiwrite(i32 noundef %0) local_unnamed_addr {
    %2 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str, i32 noundef %0)
    ret void
}

define dso_local void @builtin.iwrite(i32 noundef %0, i1 noundef zeroext %1) local_unnamed_addr {
    br i1 %1, label %3, label %4

3:
    tail call void @builtin.ciwrite(i32 noundef %0)
    br label %5

4:
    tail call void @builtin.iiwrite(i32 noundef %0)
    br label %5

5:
    ret void
}

define dso_local void @builtin.dwrite(double noundef %0) local_unnamed_addr {
    %2 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull dereferenceable(1) @.str.1, double noundef %0)
    ret void
}

; integer builtins
define fastcc i32 @builtin.iadd(i32 %a, i32 %b) {
    %result = add i32 %a, %b
    ret i32 %result
}

define fastcc i32 @builtin.isub(i32 %a, i32 %b) {
    %result = sub i32 %a, %b
    ret i32 %result
}

define fastcc i32 @builtin.imul(i32 %a, i32 %b) {
    %result = mul i32 %a, %b
    ret i32 %result
}

define fastcc i32 @builtin.idiv(i32 %a, i32 %b) {
    %result = sdiv i32 %a, %b
    ret i32 %result
}

define fastcc i32 @builtin.irem(i32 %a, i32 %b) {
    %result = srem i32 %a, %b
    ret i32 %result
}

; integer comparison
define fastcc i1 @builtin.icmp.eq(i32 %a, i32 %b) {
    %result = icmp eq i32 %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.icmp.ne(i32 %a, i32 %b) {
    %result = icmp ne i32 %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.icmp.sgt(i32 %a, i32 %b) {
    %result = icmp sgt i32 %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.icmp.sge(i32 %a, i32 %b) {
    %result = icmp sge i32 %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.icmp.slt(i32 %a, i32 %b) {
    %result = icmp slt i32 %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.icmp.sle(i32 %a, i32 %b) {
    %result = icmp sle i32 %a, %b
    ret i1 %result
}

; floating point builtins
define fastcc double @builtin.fadd(double %a, double %b) {
    %result = fadd double %a, %b
    ret double %result
}

define fastcc double @builtin.fsub(double %a, double %b) {
    %result = fsub double %a, %b
    ret double %result
}

define fastcc double @builtin.fmul(double %a, double %b) {
    %result = fmul double %a, %b
    ret double %result
}

define fastcc double @builtin.fdiv(double %a, double %b) {
    %result = fdiv double %a, %b
    ret double %result
}

; floating point comparison

define fastcc i1 @builtin.fcmp.eq(double %a, double %b) {
    %result = fcmp ueq double %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.fcmp.ne(double %a, double %b) {
    %result = fcmp une double %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.fcmp.ugt(double %a, double %b) {
    %result = fcmp ugt double %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.fcmp.uge(double %a, double %b) {
    %result = fcmp uge double %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.fcmp.ult(double %a, double %b) {
    %result = fcmp ult double %a, %b
    ret i1 %result
}

define fastcc i1 @builtin.fcmp.ule(double %a, double %b) {
    %result = fcmp ule double %a, %b
    ret i1 %result
}

; .fun code starts here
define fastcc void @new_line() {
    %tmp.0 = load { ptr, i64 }, ptr @GLOBAL.0
    tail call void @builtin.swrite({ ptr, i64 } %tmp.0)
    ret void
}

define fastcc void @skip() {
    ret void
}

define fastcc void @print_int(i32 %n) {
    %tmp.0 = alloca i1
    store i1 false, ptr %tmp.0
    %tmp.1 = load i1, ptr %tmp.0
    tail call void @builtin.iwrite(i32 %n, i1 %tmp.1)
    ret void
}

define fastcc void @print_space() {
    %tmp.0 = load { ptr, i64 }, ptr @GLOBAL.1
    tail call void @builtin.swrite({ ptr, i64 } %tmp.0)
    ret void
}

define fastcc void @print_star() {
    %tmp.0 = load { ptr, i64 }, ptr @GLOBAL.2
    tail call void @builtin.swrite({ ptr, i64 } %tmp.0)
    ret void
}

define fastcc void @print_char(i32 %ch) {
    %tmp.0 = alloca i1
    store i1 true, ptr %tmp.0
    %tmp.1 = load i1, ptr %tmp.0
    tail call void @builtin.iwrite(i32 %ch, i1 %tmp.1)
    ret void
}

define fastcc void @print_bool(i1 %b) {
    br i1 %b, label %bb.1, label %bb.0

bb.1:
    %tmp.4 = load { ptr, i64 }, ptr @GLOBAL.3
    tail call void @builtin.swrite({ ptr, i64 } %tmp.4)
    br label %bb.2

bb.0:
    %tmp.6 = load { ptr, i64 }, ptr @GLOBAL.4
    tail call void @builtin.swrite({ ptr, i64 } %tmp.6)
    br label %bb.2

bb.2:
    ret void
}

define fastcc void @print_string({ ptr, i64 } %s) {
    tail call void @builtin.swrite({ ptr, i64 } %s)
    ret void
}

@INT_SQRT_MAX = constant i32 46340

define fastcc void @spinloop(i32 %count) {
    %tmp.0 = add i32 0, 0
    %tmp.1 = tail call i1 @builtin.icmp.sle(i32 %count, i32 %tmp.0)
    br i1 %tmp.1, label %bb.3, label %bb.2

bb.3:
    br label %bb.4

bb.2:
    %tmp.6 = load { ptr, i64 }, ptr @GLOBAL.5
    tail call void @builtin.swrite({ ptr, i64 } %tmp.6)
    %tmp.8 = add i32 0, 1
    %tmp.9 = tail call i32 @builtin.isub(i32 %count, i32 %tmp.8)
    tail call void @spinloop(i32 %tmp.9)
    br label %bb.4

bb.4:
    ret void
}

define fastcc void @spinsleep(i32 %ms) {
    %tmp.0 = add i32 0, 100000
    %tmp.1 = tail call i32 @builtin.imul(i32 %ms, i32 %tmp.0)
    tail call void @spinloop(i32 %tmp.1)
    ret void
}

define fastcc void @write_ellipses(i32 %count, i32 %ms) {
    %tmp.0 = add i32 0, 0
    %tmp.1 = tail call i1 @builtin.icmp.eq(i32 %count, i32 %tmp.0)
    br i1 %tmp.1, label %bb.3, label %bb.2

bb.3:
    br label %bb.4

bb.2:
    tail call void @spinsleep(i32 %ms)
    %tmp.7 = load { ptr, i64 }, ptr @GLOBAL.6
    tail call void @builtin.swrite({ ptr, i64 } %tmp.7)
    %tmp.9 = add i32 0, 1
    %tmp.10 = tail call i32 @builtin.isub(i32 %count, i32 %tmp.9)
    tail call void @write_ellipses(i32 %tmp.10, i32 %ms)
    br label %bb.4

bb.4:
    ret void
}

define fastcc void @idbg({ ptr, i64 } %tag, i32 %n) {
    tail call void @builtin.swrite({ ptr, i64 } %tag)
    %tmp.1 = load { ptr, i64 }, ptr @GLOBAL.7
    tail call void @builtin.swrite({ ptr, i64 } %tmp.1)
    %tmp.3 = alloca i1
    store i1 false, ptr %tmp.3
    %tmp.4 = load i1, ptr %tmp.3
    tail call void @builtin.iwrite(i32 %n, i1 %tmp.4)
    %tmp.6 = add i32 0, 10
    %tmp.7 = alloca i1
    store i1 true, ptr %tmp.7
    %tmp.8 = load i1, ptr %tmp.7
    tail call void @builtin.iwrite(i32 %tmp.6, i1 %tmp.8)
    ret void
}

define fastcc void @itpldbg({ ptr, i64 } %tag, i32 %n1, i32 %n2) {
    tail call void @builtin.swrite({ ptr, i64 } %tag)
    %tmp.1 = load { ptr, i64 }, ptr @GLOBAL.8
    tail call void @builtin.swrite({ ptr, i64 } %tmp.1)
    %tmp.3 = alloca i1
    store i1 false, ptr %tmp.3
    %tmp.4 = load i1, ptr %tmp.3
    tail call void @builtin.iwrite(i32 %n1, i1 %tmp.4)
    %tmp.6 = load { ptr, i64 }, ptr @GLOBAL.9
    tail call void @builtin.swrite({ ptr, i64 } %tmp.6)
    %tmp.8 = alloca i1
    store i1 false, ptr %tmp.8
    %tmp.9 = load i1, ptr %tmp.8
    tail call void @builtin.iwrite(i32 %n2, i1 %tmp.9)
    %tmp.11 = load { ptr, i64 }, ptr @GLOBAL.10
    tail call void @builtin.swrite({ ptr, i64 } %tmp.11)
    %tmp.13 = load { ptr, i64 }, ptr @GLOBAL.0
    tail call void @builtin.swrite({ ptr, i64 } %tmp.13)
    ret void
}

define fastcc i32 @sqr(i32 %n) {
    %tmp.0 = tail call i32 @builtin.imul(i32 %n, i32 %n)
    ret i32 %tmp.0
}

define fastcc i32 @midpoint(i32 %lb, i32 %ub) {
    %tmp.0 = tail call i32 @builtin.iadd(i32 %lb, i32 %ub)
    %tmp.1 = add i32 0, 2
    %tmp.2 = tail call i32 @builtin.idiv(i32 %tmp.0, i32 %tmp.1)
    ret i32 %tmp.2
}

define fastcc i32 @intsqrtrec(i32 %lb, i32 %ub, i32 %target) {
    %tmp.0 = add i32 0, 100
    tail call void @spinsleep(i32 %tmp.0)
    %tmp.2 = load { ptr, i64 }, ptr @GLOBAL.12
    tail call void @itpldbg({ ptr, i64 } %tmp.2, i32 %lb, i32 %ub)
    %tmp.4 = add i32 0, 1
    %tmp.5 = tail call i32 @builtin.iadd(i32 %lb, i32 %tmp.4)
    %tmp.6 = tail call i1 @builtin.icmp.eq(i32 %tmp.5, i32 %ub)
    %tmp.10 = alloca i32
    br i1 %tmp.6, label %bb.8, label %bb.7

bb.8:
    store i32 %lb, ptr %tmp.10
    br label %bb.9

bb.7:
    %tmp.11 = tail call i32 @midpoint(i32 %lb, i32 %ub)
    %tmp.12 = tail call i32 @sqr(i32 %tmp.11)
    %tmp.13 = tail call i1 @builtin.icmp.sgt(i32 %tmp.12, i32 %target)
    %tmp.17 = alloca i32
    br i1 %tmp.13, label %bb.15, label %bb.14

bb.15:
    %tmp.18 = tail call i32 @midpoint(i32 %lb, i32 %ub)
    %tmp.19 = tail call i32 @intsqrtrec(i32 %lb, i32 %tmp.18, i32 %target)
    store i32 %tmp.19, ptr %tmp.17
    br label %bb.16

bb.14:
    %tmp.20 = tail call i32 @midpoint(i32 %lb, i32 %ub)
    %tmp.21 = tail call i32 @intsqrtrec(i32 %tmp.20, i32 %ub, i32 %target)
    store i32 %tmp.21, ptr %tmp.17
    br label %bb.16

bb.16:
    %tmp.22 = load i32, ptr %tmp.17
    store i32 %tmp.22, ptr %tmp.10
    br label %bb.9

bb.9:
    %tmp.23 = load i32, ptr %tmp.10
    ret i32 %tmp.23
}

define fastcc i32 @intsqrt(i32 %n) {
    %tmp.0 = load { ptr, i64 }, ptr @GLOBAL.13
    tail call void @builtin.swrite({ ptr, i64 } %tmp.0)
    %tmp.2 = alloca i1
    store i1 false, ptr %tmp.2
    %tmp.3 = load i1, ptr %tmp.2
    tail call void @builtin.iwrite(i32 %n, i1 %tmp.3)
    %tmp.5 = load { ptr, i64 }, ptr @GLOBAL.10
    tail call void @builtin.swrite({ ptr, i64 } %tmp.5)
    %tmp.7 = add i32 0, 4
    %tmp.8 = add i32 0, 500
    tail call void @write_ellipses(i32 %tmp.7, i32 %tmp.8)
    %tmp.10 = load { ptr, i64 }, ptr @GLOBAL.0
    tail call void @builtin.swrite({ ptr, i64 } %tmp.10)
    %tmp.12 = add i32 0, 0
    %tmp.13 = load i32, ptr @INT_SQRT_MAX
    %tmp.14 = tail call i32 @intsqrtrec(i32 %tmp.12, i32 %tmp.13, i32 %n)
    ret i32 %tmp.14
}

define fastcc void @main() {
    %tmp.0 = add i32 0, 625
    %tmp.1 = tail call i32 @intsqrt(i32 %tmp.0)
    %tmp.2 = alloca i1
    store i1 false, ptr %tmp.2
    %tmp.3 = load i1, ptr %tmp.2
    tail call void @builtin.iwrite(i32 %tmp.1, i1 %tmp.3)
    %tmp.5 = add i32 0, 10
    %tmp.6 = alloca i1
    store i1 true, ptr %tmp.6
    %tmp.7 = load i1, ptr %tmp.6
    tail call void @builtin.iwrite(i32 %tmp.5, i1 %tmp.7)
    ret void
}


@sbuf.GLOBAL.4 = constant [5 x i8] c"\66\61\6C\73\65"
@GLOBAL.4 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.4, i64 5 }

@sbuf.GLOBAL.7 = constant [3 x i8] c"\20\3D\20"
@GLOBAL.7 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.7, i64 3 }

@sbuf.GLOBAL.0 = constant [1 x i8] c"\0A"
@GLOBAL.0 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.0, i64 1 }

@sbuf.GLOBAL.1 = constant [1 x i8] c"\20"
@GLOBAL.1 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.1, i64 1 }

@sbuf.GLOBAL.2 = constant [1 x i8] c"\2A"
@GLOBAL.2 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.2, i64 1 }

@sbuf.GLOBAL.3 = constant [4 x i8] c"\74\72\75\65"
@GLOBAL.3 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.3, i64 4 }

@sbuf.GLOBAL.10 = constant [1 x i8] c"\29"
@GLOBAL.10 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.10, i64 1 }

@sbuf.GLOBAL.6 = constant [1 x i8] c"\2E"
@GLOBAL.6 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.6, i64 1 }

@sbuf.GLOBAL.8 = constant [4 x i8] c"\20\3D\20\28"
@GLOBAL.8 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.8, i64 4 }

@sbuf.GLOBAL.12 = constant [8 x i8] c"\28\6C\62\2C\20\75\62\29"
@GLOBAL.12 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.12, i64 8 }

@sbuf.GLOBAL.13 = constant [17 x i8] c"\63\61\6C\63\75\6C\61\74\69\6E\67\20\73\71\72\74\28"
@GLOBAL.13 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.13, i64 17 }

@sbuf.GLOBAL.5 = constant [0 x i8] c""
@GLOBAL.5 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.5, i64 0 }

@sbuf.GLOBAL.9 = constant [2 x i8] c"\2C\20"
@GLOBAL.9 = constant { ptr, i64 } { ptr @sbuf.GLOBAL.9, i64 2 }
