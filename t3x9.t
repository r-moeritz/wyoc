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