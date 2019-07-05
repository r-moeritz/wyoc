! T3X9 -> ELF-FreeBSD-386 compiler

const BPW = 4;                  ! bytes per word
const PROG_SIZE = 65536;        ! max. size of the input program
const TEXT_SIZE = 65536;
const DATA_SIZE = 65536;
const NRELOC = 10000;           ! max. no. of relocation entries
const STACK_SIZE = 100;         ! max. no. of stack elements
const SYMTBL_SIZE = 1000;
const NLIST_SIZE = 10000;
const ENDFILE = %1;             ! EOF marker

var Stack[STACK_SIZE], Sp;      ! compile stack and stack ptr.
var Line;                       ! current input line no.
var ntoa_buf::100;              ! buffer holding ASCII representation of an int

!!!!! Data conversion functions

! Convert int to string
! Returns the address of a buffer holding the ASCII representation
! of the integer x
ntoa(x) do var i, k;
    if (x = 0) return "0";
    i := 0;
    k := x<0 -> -x : x;
    while (k > 0) do
        i := i+1;
        k := k/10;
    end
    i := i+1;
    if (x < 0) i := i+1;
    ntoa_buf::i := 0;
    k := x<0 -> -x : x;
    while (k > 0) do
        i := i-1;
        ntoa_buf::i := '0' + k mod 10;
        k := k/10;
    end
    if (x < 0) do
        i := i-1;
        ntoa_buf::i := '-';
    end
    return @ntoa_buf::i;
end

!!!!! String manipulation functions

! Get the length of string s
! Returns wrong results for strings longer than 23766 chars
str.length(s) return t.memscan(s, 0, 32767);

! Copy the string ss to desination buffer sd
str.copy(sd, ss)
    t.memcopy(ss, sd, str.length(ss)+1);

! Append ss to sd
! sd must provide enough space for the combined string
str.append(sd, ss)
    t.memcopy(ss, @sd::str.length(sd),
              str.length(ss)+1);
              
! Compare strings s1 and s2 for equality
! Returns -1 if they are equal, or 0 otherwise.
str.equal(s1, s2)
    return t.memcomp(s1, s2, str.length(s1)+1) = 0;
    
!!!!! Logging functions    

! Writes a string to stdout
writes(s) t.write(1, s2, str.length(s));

! Writes a string to stderr
log(s) t.write(2, s, str.length(s));

!!!!! Error handling functions

! Write an error message m, and additional info s
! to stderr and terminate
aw(m, s) do
    log("t3x9: ");
    log(ntoa(Line));
    log(": ");
    log(m);
    if (s \= 0) do
        log(": ");
        log(s);
    end
    log("\n");
    halt 1;
end

! Log an internal error to stderr and terminate
oops(m, s) do
    log("t3x9: internal error\n");
    aw(m, s);
end

!!!!! Compile stack manipulation functions

! Push x onto the stack and increment the stack ptr.
push(x) do
    if (Sp >= STACK_SIZE) oops("stack overflow", 0);
    
    Stack[Sp] := x;
    Sp := Sp+1;
end

! Returns the top stack element
tos() return Stack[Sp-1];

! Pop the top element off the stack and return it
pop() do
    if (Sp < 1) oops("stack underflow", 0);
    
    Sp := Sp-1;
    return Stack[Sp];
end

! Swap the top two elements on the stack
swap() do var t;
    if (Sp < 2) oops("stack underflow", 0);
    t := Stack[Sp-1];
    Stack[Sp-1] := Stack[Sp-2];
    Stack[Sp-2] := t;
end

!!!!! Character class predicates

numeric(c) return '0' <= c /\ c <= '9';

alphabetic(c) return 'a' <= c /\ c <= 'z' \/
    'A' <= c /\ c <= 'Z';

!!!!! Symbol table

! Symbol table datatype
struct SYM = SNAME, SFLAGS, SVALUE;

! SFLAGS values
const GLOBF = 1;                ! global symbol
const CNST = 2;                 ! constant
const VECT = 4;                 ! vector or byte vector
const FORW = 8;                 ! forward declaration
const FUNC = 16;                ! function

var Syms[SYM*SYMTBL_SIZE];      ! symbol table
var NList::NLIST_SIZE;          ! name list

var Yp, Np;                     ! offsets of free regions in Syms and NList

! Locate symbol table entry named s, searching from the end to the beginning.
! Returns the symbol table entry, or 0 if not found.
find(s) do var i;
    i := Yp - SYM;
    while (i >= 0) do
        if (str.equals(Syms[i+SNAME], s))
            return @Syms[i];
        i := i - SYM;
    end
    return 0
end

! Lookup a symbol named s in the symbol table and verify its type against f.
! Returns the symbol table entry or reports an error and terminates.
lookup(s, f) do var y;
    y := find(s);
    if (y = 0) aw("undefined", s);
    if (y[SFLAGS] & f \= f)
        aw("unexpected type", s);
    return y;
end

! Add a name to the name list
! Returns the new name list entry
newname(s) do var k, new;
    k := str.length(s)+1;
    if (Np+k >= NLIST_SIZE
        aw("too many symbol names", s);
    new := @NList::Np;
    t.memcopy(s, new, k);
    Np := Np+k;
    return new;
end

! Add a symbol with flags f and value v to the symbol table
! Returns the new symbol table entry
add(s, f, v) do var y;
    y := find(s);
    if (y \= 0) do
        ie (y[SFLAGS] & FORW /\ f & FUNC)
            return f;
        else
            aw("redefined", s);
    end
    if (Yp+SYM >= SYMTBL_SIZE*SYM)
        aw("too many symbols", 0);
    y := @Syms[Yp];
    Yp := Yp+SYM;
    y[SNAME] := newname(s);
    y[SFLAGS] := f;
    y[SVALUE] := v;
    return y;
end

!!!!! Code Generator

! Load address of the text segment on 386-based systems
const TEXT_VADDR = 134512640; ! 8048000h

const DATA_VADDR = TEXT_VADDR + TEXT_SIZE;

! Minimal two-segment ELF header
const HEADER_SIZE = 116; ! 74h

! Size of a virtual memory page in the target OS
const PAGE_SIZE = 4096;

! A relocation entry consists of an address (RADDR) and a segment (RSEG).
struct RELOC = RADDR, RSEG;

var Rel[RELOC*NRELOC];

! These byte vectors hold the text and data segment and the ELF header
var Text_seg::TEXT_SIZE;
var Data_seg::DATA_SIZE;
var Header::HEADER_SIZE;

! The following variables indicate:
! Rp - the free region in the relocation table
! Tp - the next address in the text segment
! Dp - the next address in the data segment
! Lp - the next address in a local stack frame
! Hp - the free region in the ELF header
var Rp, Tp, Dp, Lp, Hp;

var Acc;
var Codetbl;
struct CG =
    CG_PUSH, CG_CLEAR,
    CG_LDVAL, CG_LDADDR, CG_LDLREF, CG_LDGLOB,
    CG_LDLOCL,
    CG_STGLOB, CG_STLOCL, CG_STINDR, CG_STINDB,
    CG_INCGLOB, CG_INCLOCL,
    CG_ALLOC, CG_DEALLOC, CG_LOCLVEC, CG_GLOBVEC,
    CG_INDEX, CG_DEREF, CG_INDXB, CG_DREFB,
    CG_MARK, CG_RESOLV,
    CG_CALL, CG_JUMPFWD, CG_JUMPBACK, CG_JMPFALSE,
    CG_JMPTRUE, CG_FOR, CG_FORDOWN,
    CG_ENTER, CG_EXIT, CG_HALT,
    CG_NEG, CG_INV, CG_LOGNOT, CG_ADD, CG_SUB,
    CG_MUL, CG_DIV, CG_MOD, CG_AND, CG_OR, CG_XOR,
    CG_SHL, CG_SHR, CG_EQ, CG_NEQ, CG_LT, CG_GT,
    CG_LE, CG_GE,
    CG_WORD;
