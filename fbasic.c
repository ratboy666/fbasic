/* fbasic.c
 *
 * A very simple BASIC interpreter. Quite compatible with Microsoft
 * MBASIC (CP/M) and GWBASIC (MS-DOS).
 *
 * Requires garbage collecting malloc (no free's are done)
 * There are three areas where memory can be reclaimed
 *	- random file buffers
 *	- string temporaries
 *	- variables that have been unlinked
 * Currently uses the Boehm-Demers-Weiser conservative garbage
 * collector.
 *
 * Building:
 *
 *     gcc -DUNALIGNED_DOUBLES=1 -Os -o fbasic fbasic.c -lcurses -lm -lgc
 *
 * Note that the 64 bit executable is 60K bytes... the original
 * MBASIC.COM is 20K, and is written in assembler, so we are not
 * far off the mark.
 *
 * Changes: 2.2 - Zero-trip FOR/NEXT rather than one-trip
 *              - Reformat braces
 *              - WIDTH 255 as per MBASIC 5.21
 *              - Skip OPTION statements. We currently do
 *                OPTION BASE 0, and ZEROTRIP FOR/NEXT loops
 *
 * Planned Changes:
 *              - OPTION BASE (0) 1
 *              - OPTION ZEROTRIP (ON) OFF
 *              - Variables are auto-dimensioned to 10
 *              - OPTION CRLF ON (OFF)
 *              - Tolerate double-precision syntax
 *
 * Copyright (c) 2000, 2001, 2022, Fridtjof Weigel
 * Under MIT License
 */

/* Standard headers */

/* Comment this out to build under Solaris */
#define _XOPEN_SOURCE	500

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <curses.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <sys/wait.h>

extern char **environ;

/* Garbage collecting malloc */

#include "gc.h"

/* FBASIC version */

#define VERSION		"2.2"

/* Set to 1 if your machine supports unaligned access to doubles */
/* (should be set to 1 for maximum efficiency on Intel, must be set to 0
 * on Sparc)
 */

#ifndef UNALIGNED_DOUBLES
#define UNALIGNED_DOUBLES	0
#endif

/* Set to 1 to use cr/lf, not just lf */

#ifndef CRLF
#define CRLF 0
#endif

/* Maximum array dimensions */

#define MAX_DIMS 64

/* Maximum variables to cache */

#define MAX_CACHE 5

/* Convenience defines */

#define FOREVER	for(;;)
#define NOTHING
#define YES	1
#define NO	0

/* This program reads and write mis-aligned doubles. All such transfers
 * are done through the R() and W() macros, to allow correction for
 * systems that don't support this.
 */

#if UNALIGNED_DOUBLES

/* Unaligned double access permitted */

#define R(x)	(*(double *)(x))
#define W(x,v)	(*(double *)(x) = (v))

#else

/* Read unaligned double */

static double R(unsigned char *x) {
	double v;

    /* If the pointer is aligned on 8 byte boundary, access directly */
    if ((sizeof(double) == 8) &&
        (((long int)x & 0x7) == 0))
        return *(double *)x;
    memcpy(&v, x, sizeof(double));
    return v;
}

/* Write unaligned double */

static void W(unsigned char *x, double v) {
    /* If the pointer is aligned on 8 byte boundary, access directly */
    if ((sizeof(double) == 8) &&
        (((long int)x & 0x7) == 0))
        *(double *)x = v;
    else
	memcpy(x, &v, sizeof(double));
}

#endif

/* 1K byte */

#define K		* 1024

/* Maximum program size */

#define MAX_PROGRAM	(256 K)

/* Maximum source line length */

#define MAX_SRC		1024

/* Maximum token size */

#define MAX_TKN		(MAX_SRC * 9)

/* BASIC string (length + body) */

typedef struct {
    int length;
    char *storage;
} basic_string;

/* Token */

typedef struct {
    char *name;
    unsigned char tval;
} token;

/* Token values */

#define ABS		1
#define ASC		2
#define ATN		3
#define CHAIN		4
#define MERGE		5
#define ALL		6
#define CHR		7
#define CINT		8
#define CLEAR		9
#define CLOSE		10
#define COMMON		11
#define COS		12
#define CVL		13
#define CVD		14
#define CVI		15
#define CVS		16
#define DATA		17
#define DATE		18
#define DEF		19
#define BIN		20
#define FREEFILE	21
#define FREE		22
#define FIX		23
#define DELETE		24
#define DIM		25
#define END		26
#define ENVIRONS	27
#define ENVIRON		28
#define EOFF		29
#define ERASE		30
#define ERL		31
#define ERROR		32
#define ERRF		33
#define EXP		34
#define FIELD		35
#define AS		36
#define FOR		37
#define TO		38
#define STEP		39
#define GET		40
#define GOSUB		41
#define GOTO		42
#define HEX		43
#define IF		44
#define THEN		45
#define ELSE		46
#define INPUT		47
#define INSTR		48
#define INTF		49
#define KILL		50
#define LEFT		51
#define LEN		52
#define LET		53
#define LINE		54
#define LIST		55
#define LOF		56
#define LOG		57
#define LSET		58
#define MID		59
#define MKD		60
#define MKI		61
#define MKS		62
#define NAME		63
#define NEW		64
#define NEXT		65
#define OCT		66
#define ON		67
#define OPEN		68
#define CURSESF		69
#define LOCATE		70
#define CLS		71
#define BEEP		72
#define SLEEP		73
#define POS		74
#define PRINT		75
#define USING		76
#define PUT		77
#define RANDOMIZE	78
#define READ		79
#define REM		80
#define RESTORE		81
#define RETURN		82
#define RIGHT		83
#define RND		84
#define RSET		85
#define RUN		86
#define SAVE		87
#define SGN		88
#define SIN		89
#define SPACE		90
#define SPC		91
#define SQR		92
#define STOP		93
#define STRF		94
#define STRINGF		95
#define SWAP		96
#define SYSTEM		97
#define TAB		98
#define TAN		99
#define TIME		100
#define TIMER		101
#define TROFF		102
#define TRON		103
#define VAL		104
#define WEND		105
#define WHILE		106
#define WIDTH		107
#define WRITE		108
#define CHDIR		109
#define MKDIR		110
#define RMDIR		111
#define AUTO		112
#define CSNG		113
#define CONT		114
#define SEG		115
#define EDIT		116
#define CSRLIN		117
#define FILES		118
#define INKEY		119
#define INPUTS		120
#define INP		121
#define LLIST		122
#define LPOS		123
#define LPRINT		124
#define NULLF		125
#define OUT		126
#define PEEK		127
#define POKE		128
#define RENUM		129
#define RESUME		130
#define OFF		131
#define VARPTR		132
#define WAIT		133
#define EQUAL		134
#define PLUS		135
#define DASH		136
#define STAR		137
#define SLASH		138
#define HAT		139
#define COLON		140
#define HASH		141
#define COMMA		142
#define OPENP		143
#define CLOSEP		144
#define LOAD		145
#define LOC		146
#define AND		147
#define OR		148
#define NOT		149
#define XOR		150
#define IMP		151
#define EQV		152
#define MOD		153
#define	NE		154
#define LE		155
#define GE		156
#define LT		157
#define GT		158
#define IDIV		159
#define SEMI		160
#define MKL		161
#define INT		162
#define SNG		163
#define FN		164
#define CDBL		165
#define STR		166
#define DBL		167
#define VARS		168
#define COLOR		169
#define LCASE		170
#define UCASE		171
#define LTRIM		172
#define RTRIM		173
#define INTERPRET	174
#define OPTION		175

/* Maximum token value */

#define TOKEN_MAX	200

/* Token table (name/value) */

static token token_table[] = {
    { "ABS(", ABS }, { "ASC(", ASC }, { "ATN(", ATN }, { "CHAIN", CHAIN },
    { "MERGE", MERGE }, { "ALL", ALL }, { "CHR$(", CHR }, { "CINT(", CINT },
    { "CLEAR", CLEAR }, { "CLOSE", CLOSE }, { "COMMON", COMMON },
    { "COS(", COS }, { "CVL(", CVL }, { "CVD(", CVD }, { "CVI(", CVI },
    { "CVS(", CVS }, { "DATA", DATA }, { "DATE$", DATE }, { "DEF", DEF },
    { "BIN$(", BIN }, { "FREEFILE", FREEFILE }, { "FREE", FREE },
    { "FIX(",  FIX }, { "DELETE", DELETE }, { "DIM", DIM }, { "END", END },
    { "ENVIRON$(", ENVIRONS }, { "ENVIRON", ENVIRON }, { "EOF(", EOFF },
    { "ERASE", ERASE }, { "ERL", ERL }, { "ERROR", ERROR }, { "ERR", ERRF },
    { "EXP(", EXP }, { "FIELD", FIELD }, { "AS", AS }, { "FOR", FOR },
    { "TO", TO }, { "STEP", STEP }, { "GET", GET }, { "GOSUB", GOSUB },
    { "GOTO", GOTO }, { "HEX$(", HEX }, { "IF", IF }, { "THEN", THEN },
    { "ELSE", ELSE }, { "INPUT", INPUT }, { "INSTR(", INSTR },
    { "INT(", INTF }, { "KILL", KILL }, { "LEFT$(", LEFT }, { "LEN(", LEN },
    { "LET", LET }, { "LINE", LINE }, { "LIST", LIST }, { "LOF(", LOF },
    { "LOG(", LOG }, { "LSET", LSET }, { "MID$(", MID }, { "MKD$(", MKD },
    { "MKI$(", MKI }, { "MKS$(", MKS }, { "NAME", NAME }, { "NEW", NEW },
    { "NEXT", NEXT }, { "OCT$(", OCT }, { "ON", ON }, { "OPEN", OPEN },
    { "CURSES", CURSESF }, { "LOCATE", LOCATE }, { "CLS", CLS },
    { "BEEP", BEEP }, { "SLEEP", SLEEP }, { "POS(", POS },
    { "PRINT", PRINT }, { "?", PRINT }, { "USING", USING }, { "PUT", PUT },
    { "RANDOMIZE", RANDOMIZE }, { "READ", READ }, { "REM", REM },
    { "\'", REM }, { "RESTORE", RESTORE }, { "RETURN", RETURN },
    { "RIGHT$(", RIGHT }, { "RND(", RND }, { "RSET", RSET }, { "RUN", RUN },
    { "SAVE", SAVE }, { "SGN(", SGN }, { "SIN(", SIN },
    { "SPACE$(", SPACE }, { "SPC(", SPC }, { "SQR(", SQR },
    { "STOP", STOP }, { "STR$(", STRF }, { "STRING$(", STRINGF },
    { "SWAP", SWAP }, { "SYSTEM", SYSTEM }, { "TAB(", TAB },
    { "TAN(", TAN }, { "TIME$", TIME }, { "TIMER", TIMER },
    { "TROFF", TROFF }, { "TRON", TRON }, { "VAL(", VAL }, { "WEND", WEND },
    { "WHILE", WHILE }, { "WIDTH", WIDTH }, { "WRITE", WRITE },
    { "CHDIR", CHDIR }, { "MKDIR", MKDIR }, { "RMDIR", RMDIR },
    { "CONT", CONT }, { "SEG", SEG }, { "EDIT", EDIT }, { "CSRLIN", CSRLIN},
    { "FILES", FILES }, { "INKEY$", INKEY }, { "INPUT$(", INPUTS },
    { "INP(", INP }, { "LLIST", LLIST }, { "LPOS(", LPOS },
    { "LPRINT", LPRINT }, { "NULL", NULLF }, { "OUT", OUT },
    { "PEEK(", PEEK }, { "POKE", POKE }, { "RENUM", RENUM },
    { "RESUME", RESUME }, { "OFF", OFF }, { "VARPTR(", VARPTR },
    { "WAIT", WAIT }, { "=", EQUAL }, { "+", PLUS }, { "-", DASH },
    { "^", HAT }, { "**", HAT }, { "*", STAR }, { "/", SLASH },
    { ":", COLON }, { "#", HASH }, { ",", COMMA }, { "(", OPENP },
    { "[", OPENP }, { ")", CLOSEP }, { "]", CLOSEP }, { "LOAD", LOAD },
    { "LOC(", LOC }, { "AND", AND }, { "OR", OR },
    { "NOT", NOT }, { "XOR", XOR }, { "IMP", IMP }, { "EQV", EQV },
    { "MOD", MOD }, { "<>", NE }, { "><", NE }, { "<=", LE },
    { "=<", LE }, { ">=", GE }, { "=>", GE }, { "<", LT }, { ">", GT },
    { "\\", IDIV }, { ";", SEMI }, { "MKL$(", MKL }, { "INT", INT },
    { "SNG", SNG }, { "FN", FN }, { "STR", STR }, { "DBL", DBL },
    { "VARS", VARS }, { "COLOR", COLOR }, { "COLOUR", COLOR },
    { "CSNG(", CSNG }, { "CDBL(", CDBL }, { "AUTO", AUTO }, { "][", COMMA },
    { "LCASE$(", LCASE }, { "UCASE$(", UCASE },
    { "LTRIM$(", LTRIM }, { "RTRIM$(", RTRIM },
    { "INTERPRET", INTERPRET }, { "OPTION", OPTION },
    { NULL, 0 }
};

/* "High" tokens - variables, literals, end of line, etc. */

#define VARIABLE	255
#define STRING		254
#define START		253
#define LITERAL		252
#define EOL		251
#define EOP		250
#define CONSTANT	249

#define HEX_MODE	248
#define OCTAL_MODE	247
#define BINARY_MODE	246

#define IMPOSSIBLE	245

#define UNARY_MINUS	200
#define UNARY_PLUS	201
#define UNARY_NOT	202

/* Line number ranges (low..high and default) */

#define LOW_LN		(0.0)
#define HIGH_LN		(1.0e38)
#define DEFAULT_LN	(-1.0)

/* Error values */

#define ERROR_SYNTAX	1	/* Syntax error */
#define ERROR_GOSUB	2	/* Gosub stack overflow */
#define ERROR_TYPE	3	/* Incorrect type */
#define ERROR_BREAK	4	/* Break */
#define ERROR_DATA	5	/* Out of data */
#define ERROR_NOLINE	6	/* Line number doesn't exist */
#define ERROR_WHILE	7	/* While stack overflow, nesting */
#define ERROR_FOR	8	/* For stack overflow, nesting */
#define ERROR_OM	9	/* Out of memory */
#define ERROR_REDIM	10	/* Redimension */
#define ERROR_MATH	11	/* Math error (/0, etc.) */
#define ERROR_DIM	12	/* Dimension */
#define ERROR_NOVAR	13	/* No such variable */
#define ERROR_IO	14	/* I/O error */
#define ERROR_NOEDIT	15	/* Can't edit */
#define ERROR_UNIMP	16	/* Not implemented */

/* Maximum nested gosubs, whiles and fors */

#define GOSUB_MAX	128
#define WHILE_MAX	128
#define FOR_MAX		128

/* File modes */

#define MODE_FREE	0
#define MODE_INPUT	1
#define MODE_OUTPUT	2
#define MODE_RANDOM	3
#define MODE_BINARY	4
#define MODE_APPEND	5

/* Special files */

#define CONSOLE		-1
#define PRINTER		-2
#define KEYBOARD	-3

/* Maximum open files */

#define MAX_FD		256

/* File buffer descriptor */

struct filedesc {
    char *buffer;
    int mode;
    int reclen;
    int pos;
    int width;
    FILE *f;
};

/* Variable header (name, type, pointer to value) */

struct variable {
    struct variable *next;
    char *name;
    int type;
    int common;
    void *descriptor;
};

/* Array header */

struct array {
    int ndims;
    double *dims;
    int elems;
    int type;
    void *values;
};

/* Variable types */

#define NAME_ONLY	0
#define SIMPLE_SCALAR	1
#define SIMPLE_STRING	2
#define ARRAY		3
#define STRING_ARRAY	4

/* Global variables */

/* YES if editing not allowed (un-numbered program) */
static int noedit;

/* Width of console and line printer (file widths in descriptors) */
static int width, lwidth;
static int *pwidth;

/* Output has occured -- flush is needed */
static int cout_n;

/* For timer */
static time_t current_time;
static int ontimerflag;
static int secondsleft, ontimevalue;
static unsigned char *ontimer;

/* For error dispatch */
static double onerror;
static int inerror;
static unsigned char *current_loc;

/* Are we invoked as frun? */
static int frun;

/* NULL count for output */
static int nulls;

/* Global program arguments */
static int g_ac;
static char **g_av;

/* Curses */
static WINDOW *sc;
static int use_curses;

/* Our variables */
static struct variable *variables;

/* Current device description */
static int outdev;
static FILE *current_file;
static FILE *current_infile;
static int *ppos;

/* File descriptors */
static struct filedesc files[MAX_FD] = {{0}, };

/* Token buffer for tokenizing source code */
static unsigned char tkn_buf[MAX_TKN];

/* Source buffer */
static char src[MAX_SRC];

/* Our tokenized program */
static unsigned char *program;

/* Are we tracing line numbers ? */
static int ftrace;

/* Error code and error line */
static double erl;
static double err;

static double current_line;
static int running;

/* Expression operator stack */
static unsigned char opstack[MAX_SRC];
static int opstack_ptr;

/* Expression value stack */
static double valuestack[MAX_SRC / 2];
static int valuestack_ptr;

/* Error jump */
static jmp_buf jerror;

/* Local break jump (for breaking input) */
static int jbreakflag;
static jmp_buf jbreak;

/* Output to a string */
static char *coutp;
static int coutn;
static int coutmax;

/* Cache most recently used variables.
 * The idea is to avoid lengthy searches through the variable list for
 * variables used in a single segment.
 * The variable cache must be flushed whenever names can be rebound
 * (DEF functions).
 */
static struct variable *variable_cache[MAX_CACHE] = { 0, };
static int vci = 0;

/* GOSUB stack */
static unsigned char *gosubstack[GOSUB_MAX];
static int gosubstack_ptr;

/* WHILE stack */
static unsigned char *whilestack[WHILE_MAX];
static int whilestack_ptr;

/* flag indicating break (error) has occured */
static int breakflag;

/* DATA pointer */
static unsigned char *data;

static int pos;		/* console position */
static int lpos;	/* line printer position */
static int tab;		/* tab-to location for print */

/* FOR stack */
static unsigned char *forstack[FOR_MAX];
static int forstack_ptr;

/* DEF SEG */
static int seg;

/* Variable types INT, SNG, STR, DBL, based on first character of name */
static char vtype[256] = { 0, };

/* User defined function pointers (DEF FNA..) */
static unsigned char *fns[256] = { 0, };

/* Numeric character sets */
static const char *b_set = "01";
static const char *o_set = "01234567";
static const char *t_set = "0123456789";
static const char *h_set = "0123456789ABCDEF";

/* Raise error */

static void error(int error) {
    err = error;
    erl = current_line;
    longjmp(jerror, 1);
}

/* Allocate memory, pointers contained within */

static void *get_memory(int n) {
    void *p;

    p = GC_MALLOC(n);
    if (p == NULL)
	error(ERROR_OM);
    return p;
}

/* Allocate memory, no pointers contained within */

static void *get_memory_atomic(int n) {
    void *p;

    p = GC_MALLOC_ATOMIC(n);
    if (p == NULL)
	error(ERROR_OM);
    return p;
}

/* Reallocate memory */

static void *increase_memory(void *p, int n) {
    p = GC_realloc(p, n);
    if (p == NULL)
	error(ERROR_OM);
    return p;
}

/* Free memory */

static void free_memory(void *p) {
    GC_free(p);
}

/* Force garbage collection */

static void garbage_collect(void) {
    GC_gcollect();
}

#if 0
/* Return number of garbage collections */

static int n_gc(void) {
    return GC_gc_no;
}
#endif

/* Return heap size, just data */

static int heap_size(void) {
    return GC_get_heap_size() - MAX_PROGRAM;
}

/* Check filenumber range */

static void check_fn(int filenumber) {
    if ((filenumber < 0) || (filenumber >= MAX_FD))
	error(ERROR_IO);
}

/* Return free file */

static int freefile(void) {
    int i;

    for (i = 1; i < MAX_FD; ++i)
	if (files[i].mode == MODE_FREE)
	    return i;
    error(ERROR_IO);
    return -1;
}

/* Open a file */

static void open_file(int mode, int filenumber, char *filename, int reclen) {
    char *m = "r";

    check_fn(filenumber);
    if (files[filenumber].mode != MODE_FREE)
	error(ERROR_IO);
    switch (mode) {
        case MODE_INPUT:	m = "r"; break;
	case MODE_OUTPUT:	m = "w"; break;
	case MODE_APPEND:	m = "a+"; break;
	case MODE_BINARY:	m = "r+b"; break;
	case MODE_RANDOM:
			files[filenumber].reclen = reclen;
			files[filenumber].buffer = get_memory_atomic(reclen);
			m = "r+b";
			break;
    }
    files[filenumber].f = fopen(filename, m);
    if (files[filenumber].f == NULL) {
        if (mode == MODE_RANDOM) {
	    free_memory(files[filenumber].buffer);
	    files[filenumber].buffer = NULL;
	}
	error(ERROR_IO);
    }
    files[filenumber].mode = mode;
    files[filenumber].pos = 0;
    files[filenumber].width = 0;
}

/* Close a file */

static void close_file(int filenumber) {
    check_fn(filenumber);
    if (files[filenumber].mode == MODE_FREE)
	return;
    if (files[filenumber].mode == MODE_RANDOM)
	free_memory(files[filenumber].buffer);
    files[filenumber].buffer = NULL;
    files[filenumber].mode = MODE_FREE;
    fclose(files[filenumber].f);
    files[filenumber].f = NULL;
}

/* Return FILE* for file */

static FILE *pfile(int filenumber) {
    FILE *f;

    check_fn(filenumber);
    f = files[filenumber].f;
    if (f == NULL)
	error(ERROR_IO);
    return f;
}

/* Select file for i/o */

static void select_file(int filenumber) {
    coutp = NULL;
    if (outdev == filenumber)
	return;
    outdev = filenumber;
    if (filenumber == CONSOLE) {
	current_file = stdout;
	current_infile = stdin;
	ppos = &pos;
	pwidth = &width;
    } else if (filenumber == PRINTER) {
	current_file = stderr;
	current_infile = stdin;
	ppos = &lpos;
	pwidth = &lwidth;
    } else {
	current_file = pfile(filenumber);
	current_infile = current_file;
	ppos = &(files[filenumber].pos);
	pwidth = &(files[filenumber].width);
    }
}

/* Refresh for curses */

static void refresh_screen(void) {
    if (use_curses)
	refresh();
}

/* Flush output */

static void flush_out(void) {
    if (cout_n) {
	cout_n = 0;
	fflush(stdout);
	refresh_screen();
    }
}

/* Check for BREAK */

static void check_break(void) {
    if (breakflag) {
	breakflag = 0;
	error(ERROR_BREAK);
    }
}

/* Get key */

static int get_key(void) {
    unsigned char key;

    flush_out();
    while (read(1, &key, 1) < 0)
	check_break();
    check_break();
    return key;
}

/* Get line */

static void get_line(char *s, int n) {
    int x, y;

    s[0] = '\0';
    if (use_curses && (current_infile == stdin)) {
	if (setjmp(jbreak) == 0) {
	    jbreakflag = YES;
	    /* getnstr(s,n) is not available on Solaris */
	    wgetnstr(sc, s, n);
	    /* Needed for Solaris - if an empty string is accepted
	     * by wgetnstr on Solaris, the cursor position is not
	     * changed.
	     */
	}
	jbreakflag = NO;
	echo();
	nocbreak();
	getyx(sc, y, x);
	move(y, x);
	refresh();
	check_break();
	return;
    }
    if (fgets(s, n, current_infile) == NULL) {
	check_break();
	error(ERROR_IO);
    }
    check_break();
}

/* Output character */

static void crlf(void);

static void cout(char c) {
    if (coutp) {
	if (coutn >= coutmax) {
	    coutmax += 1024;
	    coutp = increase_memory(coutp, coutmax);
	}
	coutp[coutn++] = c;
	return;
    }

    ++cout_n;

    if (use_curses && (current_file == stdout)) {
	if (addch(c) == ERR)
	    error(ERROR_IO);
	    /* WIDTH not effective in CURSES mode */
	    return;
    }

    if (putc(c, current_file) == EOF)
	error(ERROR_IO);

    if (c == 8)
	--*ppos;
    else if (c == 13)
	*ppos = 0;
    else if (c == 10)
	*ppos = 0;
    else if (c != 7)
	++*ppos;

    if ((*pwidth > 0) && (*ppos >= *pwidth))
	crlf();
}

/* String out */

static void sout(char *s) {
    while (*s)
	cout(*s++);
}

/* ANSI sequence to clear screen and home cursor */

static void ansi_cls(void) {
    sout("\x1b[1;1H\x1b[J");
}

/* Output return,newline */

static void crlf(void) {
    int i;

#if CRLF
    sout("\r\n");
#else
    cout('\n');
#endif
    for (i = 0; i < nulls; ++i)
	cout(0);
}

/* Output return,newline for edit */

static void ncrlf(void) {
    if (use_curses)
	cout('\n');
    else
	sout("\r\n");
}

/* Execute program with pty */

static void pty(char *cmd) {
    int i;
    char data;
    int master = 0, slave;
    pid_t pid_sh;

#if 0

    /* BSD style pty -- Linux 2.0 needs this */

    char mastername[32], slavename[32];
    char *s, *t;
    int found;

    found = NO;
    for (s = "pqrs"; !found && *s; ++s) {
	for (t = "0123456789abcdef"; !found && *t; ++t) {
	    sprintf(mastername, "/dev/pty%c%c", *s, *t);
	    sprintf(slavename, "/dev/tty%c%c", *s, *t);
	    if ((master = open(mastername, O_RDWR|O_NDELAY|O_SYNC))
			    >= 0)
		found = YES;
	}
    }
    if (!found) {
	fprintf(stderr, "all ptys in use\n");
	return;
    }
#else

    /* SVR4 style pty */

    char *slavename;

    if ((master = open("/dev/ptmx", O_RDWR|O_NDELAY|O_SYNC)) < 0) {
	perror("/dev/ptmx");
	return;
    }
    if (grantpt(master) < 0) {
	perror("grantpt");
	close(master);
	return;
    }
    if (unlockpt(master) < 0) {
	perror("unlockpt");
	close(master);
	return;
    }
    slavename = ptsname(master);
    if (slavename == NULL) {
	perror("ptsname");
	close(master);
	return;
    }

#endif

    pid_sh = fork();
    if (pid_sh < 0) {
	perror("fork");
	return;
    }
    if (pid_sh == 0) {
	setsid();
	slave = open(slavename, O_RDWR);
	dup2(slave, 0);
	dup2(slave, 1);
	dup2(slave, 2);
	/* close(slave); */
	/* close all other fds (including slave) */
	for (i = 3; i < 256; ++i)
	    close(i);
	system(cmd);
	close(0);
	close(1);
	close(2);
	exit(0);
    } else {
	/* XXX use select to make this more efficient */
	/* XXX sending break to sub-process??? */
	noecho();
	cbreak();
	nodelay(sc, TRUE);
	FOREVER {
	    i = read(master, &data, 1);
	    if ((i < 0) && (errno != EAGAIN))
		break;
	    if (i == 1)
		if (data != '\r')
		    cout(data);

	    i = getch();
	    data = i;
	    if (i != ERR) {
		do {
		    i = write(master, &data, 1);
		    if ((i < 0) && (errno != EAGAIN))
			break;
		} while (i != 1);
		if (i < 0)
		    break;
	    }
	}
	close(master);
	while (wait(NULL) != pid_sh)
	    ;
	echo();
	nocbreak();
	nodelay(sc, FALSE);
    }
}

/* Execute program */

static void external_execute(char *prog, char *args) {
    char *buf;

    if (args == NULL)
	args = "";
    buf = get_memory_atomic(strlen(prog) + strlen(args) + 1);
    strcpy(buf, prog);
    if (*args != '\0') {
	strcat(buf, " ");
	strcat(buf, args);
    }
    if (use_curses)
	pty(buf);
    else
	system(buf);
    free_memory(buf);
}

/* Enter raw keyboard mode */

static void raw_keyboard(void) {
    external_execute("stty", "-echo raw");
}

/* Resume normal keyboard mode */

static void cook_keyboard(void) {
    external_execute("stty", "echo cooked");
}

/* Is character alpha? */

static int is_alpha(char c) {
    return ('A' <= c) && (c <= 'Z');
}

/* Is character alpha or digit? */

static int is_alphanum(char c) {
    return ((('A' <= c) && (c <= 'Z')) ||
	    (('0' <= c) && (c <= '9')));
}

/* Is variable a typed variable? */

static int is_type_variable(unsigned char *p, char t) {
    char type;

    if (*p == VARIABLE) {
	type = (p+2)[p[1]-2];
	if (type == t)
	    return YES;
	if (is_alphanum(type)) {
	    if (vtype[*(p+2)] == t)
		return YES;
	}
    }
    return NO;
}

/* Is variable a string variable? */

static int is_string_variable(unsigned char *p) {
    return is_type_variable(p, '$');
}

/* Is variable an integer variable? */

static int is_integer_variable(unsigned char *p) {
    return is_type_variable(p, '%');
}

/* Return error message */

static char *error_message(void) {
    switch ((int)err) {
	case ERROR_SYNTAX:	return "syntax";
	case ERROR_GOSUB:	return "gosub stack";
	case ERROR_BREAK:	return "break";
	case ERROR_DATA:	return "out of data";
	case ERROR_NOLINE:	return "no such line";
	case ERROR_WHILE:	return "while stack";
	case ERROR_FOR:		return "for stack";
	case ERROR_OM:		return "out of memory";
	case ERROR_REDIM:	return "redimension";
	case ERROR_MATH:	return "arithmetic";
	case ERROR_DIM:		return "dimension";
	case ERROR_TYPE:	return "data type";
	case ERROR_NOVAR:	return "no such variable";
	case ERROR_IO:		return "io";
	case ERROR_NOEDIT:	return "can\'t edit";
	case ERROR_UNIMP:	return "not implemented";
	default: 		return "unknown";
    }
}

/* Signal handler for break */

static void signal_break(int sig) {
    int olddev = outdev;

    select_file(CONSOLE);
    sout("^C");
    crlf();
    flush_out();
    signal(SIGINT, signal_break);
    select_file(olddev);
    breakflag = YES;
    if (jbreakflag)
	longjmp(jbreak, 1);
}

/* Peek memory */

static unsigned char peek(size_t n) {
    unsigned char *p = (unsigned char *)n;

    return *p;
}

/* Poke memory */

static void poke(size_t n, unsigned char v) {
    unsigned char *p = (unsigned char *)n;

    *p = v;
}

/* In from port */

static unsigned char inp(int n) {
    return 0;
}

/* Out to port */

static void out(int n, unsigned char v) {
}

/* Wait for port */

static void waitport(int port, int and, int xor) {
    while (((inp(port) ^ xor) & and) == 0)
	check_break();
}

/* Create a variable */

static void create_variable(char *name, void *descriptor, int t) {
    struct variable *p;

    p = get_memory(sizeof(struct variable));
    p->name = strdup(name);
    p->descriptor = descriptor;
    p->type = t;
    p->common = NO;
    p->next = variables;
    variables = p;
}

/* Find variable */

static struct variable *find_variable(char *name) {
    struct variable *p;
    char c;
    char tvar[128];
    int n;

    for (n = 0; n < MAX_CACHE; ++n) {
	p = variable_cache[n];
	if (p && (strcmp(p->name, name) == 0))
	    return p;
    }
    for (p = variables; p; p = p->next)
	if (strcmp(p->name, name) == 0) {
	    variable_cache[vci] = p;
	    if (++vci >= MAX_CACHE)
		vci = 0;
	    return p;
	}
	if (vtype[(unsigned char)*name]) {
	    /* handle case like: i% = 1:def int i: i = 0 */
	    /* XXX probably don't have to worry */
	    n = strlen(name);
	    c = name[n - 1];
	    if (is_alphanum(c)) {
		strcpy(tvar, name);
		tvar[n] = vtype[(unsigned char)*name];
		tvar[n+1] = 0;
		for (p = variables; p; p = p->next)
		    if (strcmp(p->name, tvar) == 0)
			return p;
	    }
	}
    return NULL;
}

/* Flush (empty) the variable cache */

static void flush_variable_cache(void) {
    int n;

    for (n = 0; n < MAX_CACHE; ++n)
	variable_cache[n] = NULL;
}

/* Destroy variable */

static void destroy_variable(struct variable *v) {
    struct variable *p;

    if (v == variables)
	variables = variables->next;
    else {
	for (p = variables; p->next != v; p = p->next)
	    ;
	p->next = v->next;
    }
    flush_variable_cache();
    /* XXX Now variable v is ready for freeing */
}

/* Kill all non-common variables */

static void only_common_variables(void) {
    struct variable *p, *q;
    struct variable *common_variables = NULL;

    flush_variable_cache();
    for (p = q = variables; q; p = q) {
	q = p->next;
	if (p->common) {
	    p->next = common_variables;
	    common_variables = p;
	}
    }
    variables = common_variables;
}

/* Return NULL, or pointer to descriptor */

static void *test_type(char *name, int type) {
    struct variable *pv;

    pv = find_variable(name);
    if (pv != NULL) {
	if (pv->type == NAME_ONLY) {
	    pv->type = type;
	    if (type == SIMPLE_SCALAR)
		pv->descriptor = get_memory_atomic(
						sizeof(double));
	    if (type == SIMPLE_STRING)
		pv->descriptor = get_memory(
						sizeof(basic_string));
	}
	if (pv->type != type)
	    error(ERROR_TYPE);
	return pv->descriptor;
	}
    return NULL;
}

/* Return address of simple scalar */

static double *address_scalar(char *name) {
    double *ps;

    ps = test_type(name, SIMPLE_SCALAR);
    if (ps != NULL)
	return ps;
    ps = get_memory_atomic(sizeof(double));
    *ps = 0.0;
    create_variable(name, ps, SIMPLE_SCALAR);
    return ps;
}

/* Assign simple scalar variable */

static void set_scalar(char *name, double v) {
    *address_scalar(name) = v;
}

/* Retrieve simple scalar variable */

static double get_scalar(char *name) {
    return *address_scalar(name);
}

/* Create string variable */

static void create_string(char *name, basic_string v) {
    basic_string *ps;

    ps = test_type(name, SIMPLE_STRING);
    if (ps != NULL) {
	*ps = v;
	return;
    }
    ps = get_memory(sizeof(basic_string));
    *ps = v;
    create_variable(name, ps, SIMPLE_STRING);
}

/* Assign simple string variable */

static void set_string(char *name, basic_string v) {
    char *buf;

    buf = get_memory_atomic(v.length + 1);
    memcpy(buf, v.storage, v.length);
    v.storage = buf;
    create_string(name, v);
}

/* Address of simple string variable */

static basic_string *address_string(char *name) {
    basic_string *ps;
    basic_string s;

    ps = test_type(name, SIMPLE_STRING);
    if (ps != NULL)
	return ps;
    else {
	s.length = 0;
	s.storage = NULL;
	set_string(name, s);
	return test_type(name, SIMPLE_STRING);
    }
}

/* Retrieve simple string */

static basic_string get_string(char *name) {
    return *address_string(name);
}

/* Create an array */

static void create_array(char *name, int t, int dptr, double *dimensions) {
    struct variable *pv;
    struct array *a;
    int i, n;
    int cmn = NO;

    pv = find_variable(name);
    if (pv != NULL) {
	if (pv->type == NAME_ONLY)
	    cmn = pv->common;
	if ((pv->type == NAME_ONLY) || (pv->type == SIMPLE_SCALAR) ||
	    (pv->type == SIMPLE_STRING))
	    destroy_variable(pv);
	else
	    error(ERROR_REDIM);
    }
    a = get_memory(sizeof(struct array));
    a->ndims = dptr;
    a->dims = get_memory_atomic(dptr * sizeof(double));
    memcpy(a->dims, dimensions, dptr * sizeof(double));
    n = 1;
    for (i = 0; i < dptr; ++i)
	n *= dimensions[i] + 1;
    a->elems = n;
    a->type = t;
    if (t == ARRAY)
	a->values = get_memory_atomic(n * sizeof(double));
    else /* t == STRING_ARRAY */
	a->values = get_memory(n * sizeof(basic_string));
    create_variable(name, a, t); /* ARRAY or STRING_ARRAY */
    find_variable(name)->common = cmn;
}

/* Convert string to numeric. String must have terminal '\0' */

static double cvt_base(char **p, const char *set, int base) {
    int n;
    char *s;
    double value = 0.0;

    while (**p && (s = strchr(set, **p))) {
	n = s - set;
	++(*p);
	value = value * base;
	value = value + n;
    }
    return value;
}

/* Return numeric value of string */

static double val(char **p, int bcerr, int *pbase) {
    double value, frac;
    int exp, exp_sign;
    const char *set;
    char *savep = *p;

    *pbase = 10;
    value = 0.0;
    if (**p == '&') {
	++(*p);
	if (**p == 'O') {
	    *pbase = 8;
	    set = o_set;
	} else if (**p == 'H') {
	    *pbase = 16;
	    set = h_set;
	} else if (**p == 'B') {
	    *pbase = 2;
	    set = b_set;
	} else if (**p == 'T') {
	    *pbase = 10;
	    set = t_set;
	} else {
	    if (bcerr)
		error(ERROR_SYNTAX);
	    else {
		*p = savep;
		return 0;
	    }
	}
	++(*p);
	    return cvt_base(p, set, *pbase);
    }

    set = t_set;
    if (**p && (strchr(set, **p) || (**p == '.'))) {
	value = cvt_base(p, set, 10);
	if (**p == '.') {
	    ++(*p);
	    if (**p && strchr(set, **p)) {
		/* We should be more accurate - try
		 * a table
		 */
		frac = cvt_base(p, set, 10);
		while (frac >= 1.0)
		    frac /= 10.0;
		value += frac;
	    }
	}
	if ((**p == 'E') || (**p == 'D')) {
	    ++(*p);
	    if ((**p == '-') || (**p == '+') ||
		(('0' <= **p) && (**p <= '9')))
		;
	    else {
		/* E or D was false alarm */
		--(*p);
		return value;
	    }

	    exp_sign = 1;
	    if (**p == '-') {
		exp_sign = -1;
		++(*p);
	    } else if (**p == '+') {
		++(*p);
	    }
	    exp = 0;
	    if (**p && strchr(set, **p)) {
		exp = cvt_base(p, set, 10);
		value = value * pow(10.0, exp * exp_sign);
	    }
	}
    }
    return value;
}

/* Skip over variable */

static unsigned char *skip_variable(unsigned char *p) {
    p += 2;
    p += p[-1];
    return p;
}

/* Return variable name */

static char *variable_name(unsigned char *p) {
    return (char *)p+2;
}

/* Given pointer to packed line and original line, find original text
 * (used for REM).
 */

static char *find_original(char *dst, char *src, char *hit) {
    char *k = dst;

    while (*src) {
	if (dst >= hit)
	    return src;
	if (*src == '\"') {
	    ++dst;
	    ++src;
	    while (*src && (*src != '\"')) {
		++dst;
		++src;
	    }
	    if (*src) {
		++src;
		++dst;
	    }
	} else if (*src <= ' ') {
	    if ((dst == k) || (dst[-1] == ' '))
		;
	    else
		++dst;
	    ++src;
	} else {
	    ++dst;
	    ++src;
	}
    }
    return src;
}

/* Pack input line, prior to tokenizing. Copy strings intact ("..."),
 * remove all spaces outside of strings, and uppercase everything
 * not in a string.
 */

static void pack_line(char *dst, char *src) {
    char *k = dst;

    while (*src) {
	if (*src == '\"') {
	    *dst++ = '\"';
	    ++src;
	    while (*src && (*src != '\"'))
		*dst++ = *src++;
	    if (*src) {
		++src;
		*dst++ = '\"';
	    }
	} else if (*src <= ' ') {
	    if ((dst == k) || (dst[-1] == ' '))
		;
	    else
		*dst++ = ' ';
	    ++src;
	} else if (('a' <= *src) && (*src <= 'z'))
	    *dst++ = *src++ - 'a' + 'A';
	else
	    *dst++ = *src++;
    }
    *dst = '\0';
}

/* Find longest token that matches input (return -1 if none) */

static int match_token(char **p) {
    int m, n, i, l;

    if (**p == '\0')
	return -1;
    l = -1;
    m = -1;
    for (i = 0; token_table[i].name; ++i) {
	n = strlen(token_table[i].name);
	if (n <= strlen(*p)) {
	    /* If its DEF, or FN, we allow the match.
	     * This allows DEFINT, DEF INT to be the same
	     */
	    if (((token_table[i].tval == FN) ||
		 (token_table[i].tval == DEF)) &&
		(strncmp(*p, token_table[i].name, n) == 0)) {
		if (n > m) {
		    m = n;
		    l = i;
		}
	    }
	    /* We must match the token, and there must a non-alpha
	     * following, except if the token isn't a word.
	     */
	    if ((strncmp(*p, token_table[i].name, n) == 0) &&
		(
		 (!is_alpha(**p)) ||	/* token not alpha */
		 ((*p)[n-1] == '(') ||	/* token is func */
		 (!is_alpha((*p)[n]))	/* non alpha delim */
		)) {
		if (n > m) {
		    m = n;
		    l = i;
		}
	    }
	}
    }
    if (l != -1) {
	*p += strlen(token_table[l].name);
	return token_table[l].tval;
    }
    return -1;
}

/* Match variable name in source */

static int match_variable(char **p, char *vn) {
    if (is_alpha(**p)) {
	while (is_alphanum(**p) || (**p == '.') || (**p == '_')) {
	    *vn++ = **p;
	    ++(*p);
	}
	if ((**p == '$') || (**p == '%') || (**p == '!')) {
	    *vn++ = **p;
	    ++(*p);
	}
	*vn = '\0';
	    return YES;
    }
    return NO;
}

/* Match constant (string, number).
 * 
 * A bare '.' comes out as the numeric constant 0. This is probably
 * not worth fixing.
 */

static int match_constant(char **p, char *s, int *pbase) {
    double value;
    char *oldp = *p;

    if (**p == '\"') {
	++(*p);
	while (**p && (**p != '\"')) {
	    *s++ = **p;
	    ++(*p);
	}
	*s = '\0';
	if (**p)
	    ++(*p);
	return STRING;
    }

    value = val(p, 1, pbase);
    if (*p != oldp) {
	W((unsigned char *)s, value);
	return CONSTANT;
    }

    return NO;
}

/* Convert double to long integer */

static long int d_to_li(double x) {
    long int ix;

    if (x >= 0x80000000) {
	ix = x - 0x80000000;
	ix += 0x80000000;
    } else
	ix = x;
    return ix;
}

/* List tokenized line -- untokenizes line p to string s */
	
static void list_line(unsigned char *pt, char *s) {
    int i;
    long int n;
    char c;
    unsigned int b;
    unsigned char token, last_token;
    int base;

    base = 10;
    if (*pt == CONSTANT) {
	/* line number */
	++pt;
	s += sprintf(s, "%8g", R(pt));
	pt += sizeof(double);
    } else
	s += sprintf(s, "         ");
    token = 0;
    while (*pt != EOL) {
	last_token = token;
	token = *pt;
	if (token < TOKEN_MAX) {
	    for (i = 0; token_table[i].name; ++i)
		if (token == token_table[i].tval)
		    break;
	    n = strlen(token_table[i].name);
	    c = token_table[i].name[n - 1];
	    if ((c != '(') &&
		is_alpha(token_table[i].name[0]) &&
		(s[-1] != ' ') && (s[-1] != ';') &&
		(s[-1] != ':') && (s[-1] != ','))
		s += sprintf(s, " ");
	    /* MID$ is the only function that can start a line */
	    if ((token == MID) && (last_token == 0))
		s += sprintf(s, " ");
	    s += sprintf(s, "%s", token_table[i].name);
	    if (is_alpha(c) && (token != REM) && (token != FN))
		s += sprintf(s, " ");
	    ++pt;
	} else if (token == LITERAL) {
	    s += sprintf(s, "%s", variable_name(pt));
	    pt = skip_variable(pt);
	} else if (token == VARIABLE) {
	    if (is_alphanum(s[-1]) && (last_token != FN))
		s += sprintf(s, " ");
	    s += sprintf(s, "%s", variable_name(pt));
	    pt = skip_variable(pt);
	} else if (token == STRING) {
	    s += sprintf(s, "\"%s\"", variable_name(pt));
	    pt = skip_variable(pt);
	} else if (token == BINARY_MODE) {
	    ++pt;
	    base = 2;
	    s += sprintf(s, "&B");
	} else if (token == OCTAL_MODE) {
	    ++pt;
	    base = 8;
	    s += sprintf(s, "&O");
	} else if (token == HEX_MODE) {
	    ++pt;
	    base = 16;
	    s += sprintf(s, "&H");
	} else if (token == CONSTANT) {
	    ++pt;
	    if (base == 10)
		s += sprintf(s, "%.10G", R(pt));
	    else {
		n = d_to_li(R(pt));
		if (base == 2) {
		    if (n == 0)
			s += sprintf(s, "0");
		    else {
			b = 0x80000000;
			while ((b & n) == 0)
			    b >>= 2;
			while (b != 0) {
			    s += sprintf(s, "%s",
				       (b & n) ? "1" : "0");
			    b >>= 2;
			}
		    }
		}
	    else if (base == 8)
		s += sprintf(s, "%lo", n);
	    else if (base == 16)
		s += sprintf(s, "%lX", n);
	    }
	pt += sizeof(double);
	base = 10;
	}
    }
}

/* Tokenize source line. The tokenized line is terminated with EOL EOP.
 * The EOP is added so that if the line is executed, control is returned
 * to the command loop on line end.
 */

static void tokenize_line(char *src, unsigned char *pt) {
    char d[MAX_SRC];
    char *p;
    int n, tkn;
    char buf[MAX_SRC];
    char *remark;
    int base;

    pack_line(d, src);
    p = d;
    while (*p) {
	if (*p == ' ')
	    ++p;
	else if ((tkn = match_token(&p)) != -1) {
	    *pt++ = tkn;
	    if (tkn == REM) {
		/* remark, followed by LITERAL n comment 0 */
		*pt++ = LITERAL;
		remark = find_original(d, src, p);
		while (strlen(remark) &&
		       (remark[strlen(remark)-1] < ' '))
		    remark[strlen(remark)-1] = '\0';
		n = strlen(remark) + 1;
		*pt++ = n;
		strcpy((char *)pt, remark);
		pt += n;
		d[0] = '\0';
		p = d;
	    }
	} else if (match_variable(&p, buf)) {
	    /* VARIABLE n name 0 */
	    *pt++ = VARIABLE;
	    n = strlen(buf) + 1;
	    *pt++ = n;
	    strcpy((char *)pt, buf);
	    pt += n;
	} else if ((n = match_constant(&p, buf, &base)) != 0) {
	    if (n == STRING) {
		/* STRING n constant 0 */
		*pt++ = STRING;
		n = strlen(buf) + 1;
		*pt++ = n;
		strcpy((char *)pt, buf);
		pt += n;
	    } else if (n == CONSTANT) {
		/* CONSTANT double */
		if (base == 2)
		    *pt++ = BINARY_MODE;
		else if (base == 8)
		    *pt++ = OCTAL_MODE;
		else if (base == 16)
		    *pt++ = HEX_MODE;
		*pt++ = CONSTANT;
		W(pt, R((unsigned char *)buf));
		pt += sizeof(double);
	    }
	} else
	    error(ERROR_SYNTAX);
    }
    *pt++ = EOL;
    *pt = EOP;
}

/* Skip one element */

static unsigned char *skip_one(unsigned char *p)
	{
	if (*p < TOKEN_MAX)
		++p;
	else if (*p == CONSTANT)
		p += sizeof(double) + 1;
	else if ((*p == EOL) || (*p == HEX_MODE) ||
				(*p == OCTAL_MODE) ||
				(*p == BINARY_MODE))
		++p;
	else /* LITERAL,VARIABLE,STRING */
		p = skip_variable(p);
	return p;
	}

/* Skip primitive, skip to one of three tokens */

static unsigned char *skip_prim(unsigned char *p, unsigned char opt,
					   unsigned char opt2,
					   unsigned char opt3) {
    while ((*p != EOP) && (*p != opt) && (*p != opt2) && (*p != opt3))
	p = skip_one(p);
    return p;
}

/* Skip to new statement */

static unsigned char *skip_statement(unsigned char *p) {
    return skip_prim(p, EOL, COLON, IMPOSSIBLE) + 1;
}

/* Skip past EOL */

static unsigned char *skip(unsigned char *p) {
    return skip_prim(p, EOL, IMPOSSIBLE, IMPOSSIBLE) + 1;
}

/* Skip past ELSE */

static unsigned char *skip_else(unsigned char *p) {
    return skip_prim(p, EOL, ELSE, IMPOSSIBLE) + 1;
}

/* Skip to DATA */

static unsigned char *skip_data(unsigned char *p) {
    return skip_prim(p, DATA, IMPOSSIBLE, IMPOSSIBLE);
}

/* Skip to WHILE/WEND */

static unsigned char *skip_to_while_wend(unsigned char *p) {
    return skip_prim(p, EOL, WHILE, WEND);
}

/* Skip to FOR/NEXT */

static unsigned char *skip_to_for_next(unsigned char *p) {
    return skip_prim(p, EOL, FOR, NEXT);
}

/* Return start of program */

static unsigned char *sop(void) {
    return program+1;
}

/* Find line, or next highest */

static unsigned char *find_line(double n, int early) {
    unsigned char *p;
    double t;

    for (p = sop(); *p != EOP; ) {
	if (*p == CONSTANT) {
	    ++p;
	    t = R(p);
	    if (t == n)
		return p - 1;
	    else if (early && (t > n))
		return p - 1;
	    p += sizeof(double);
	}
	p = skip(p);
    }
    return p;	/* end of program */
}

/* Find line, if it doesn't exist, then error */

static unsigned char *find_line_strict(double n) {
    unsigned char *p = find_line(n, NO);

    if (*p == EOP)
	error(ERROR_NOLINE);
    if (R(p+1) != n)
	error(ERROR_NOLINE);
    return p;
}

/* Return end of program */

static unsigned char *eop(void) {
    unsigned char *p = sop();

    while (*p != EOP)
	p = skip(p);
    ++p;
    return p;
}

/* Delete line from program */

static void delete_line(unsigned char *pp) {
    unsigned char *e = skip(pp);
    unsigned char *x = eop();

    memmove(pp, e, x - e);
}

/* Insert line into program */

static void insert_line(unsigned char *p, unsigned char *pp) {
    unsigned char *x = eop();
    unsigned char *e = skip(p);
    int amount;

    amount = e - p;
    memmove(pp + amount, pp, x - pp);
    memmove(pp, p, amount);
}

/* Clear stacks, leave variables intact */

static void clear_execution_state(void) {
    coutp = NULL;
    coutn = 0;
    coutmax = 0;
    outdev = -99;
    select_file(CONSOLE);
    width = 0;
    lwidth = 0;
    ppos = &pos;
    pwidth = &width;
    cout_n = 0;
    ontimerflag = NO;
    ontimer = NULL;
    onerror = 0;
    inerror = NO;
    opstack_ptr = 0;
    valuestack_ptr = 0;
    gosubstack_ptr = 0;
    whilestack_ptr = 0;
    forstack_ptr = 0;
    breakflag = 0;
    pos = 0;
    lpos = 0;
    tab = 0;
    data = sop();
    current_loc = NULL;
    err = 0;
    flush_variable_cache();
}

/* Edit program (delete and/or insert line) */

static void edit_program(unsigned char *p) {
    double t, o;
    unsigned char *pp;

    clear_execution_state();
    if (noedit) {
	insert_line(p, eop()-1);
	return;
    }
    t = R(p + 1);
    current_line = t;
    pp = find_line(t, YES);
    if (*pp == CONSTANT) {
	o = R(pp + 1);
	if (t == o)
	    delete_line(pp);
    }
    /* line number by itself deletes a line */
    if (*(p+sizeof(double)+1) != EOL)
	insert_line(p, pp);
}

/* Return true if at end of statement */

static int statement_end(unsigned char *t) {
    return (*t == EOL) || (*t == COLON);
}

/* Analyze two line numbers:
 *
 *	l1	l1-	-l2	l1-l2
 */

static unsigned char *two_ln(unsigned char *t, double *x1, double *x2) {
    *x1 = DEFAULT_LN;
    *x2 = DEFAULT_LN;
    if (*t == CONSTANT) {
	*x1 = R(t + 1);
	if (*x1 == 0)
	    *x1 = current_line;
	current_line = *x1;
	t += sizeof(double) + 1;
    }
    if (*t == DASH) {
	if (*x1 == DEFAULT_LN)
	    *x1 = LOW_LN;
	++t;
	if (*t == CONSTANT) {
	    *x2 = R(t + 1);
	    t += sizeof(double) + 1;
	} else
	    *x2 = HIGH_LN;
    } else
	*x2 = DEFAULT_LN;
    if (!statement_end(t))
	error(ERROR_SYNTAX);
    return t;
}

/* Simple interactive line editor */

static void edit(unsigned char *p) {
    char src[MAX_SRC];
    char *s, *ep, *t, key;
    int n, f;

    raw_keyboard();
    list_line(p, src);
retype:
    sout(src);
    ncrlf();
again:
    s = src;
    while (*s == ' ')
	cout(*s++);
    while (*s != ' ')
	cout(*s++);
    cout(*s++);
    ep = s;
    n = 0;
    FOREVER {
	key = get_key();
	if (n < 0)
	    n = 0;
	if (key == ' ') {
	    do
		if (*ep)
		    cout(*ep++);
	    while (--n > 0);
	} else if ((key == 'a') || (key == 'A')) {
	    ncrlf();
	    list_line(p, src);
	    goto again;
	} else if ((key == 'h') || (key == 'H')) {
	    *ep = '\0';
	    goto insert;
	} else if (key == 0x1b) {
	    n = 0;
	} else if (('0' <= key) && (key <= '9')) {
	    n = n * 10 + (key - '0');
	} else if ((key == '\r') || (key == 'e') || (key == 'E')) {
end_edit:
	    while (*ep)
		cout(*ep++);
	    ncrlf();
	    tokenize_line(src, tkn_buf);
	    edit_program(tkn_buf);
	    break;
	} else if ((key == 3) || (key == 'q') || (key == 'Q')) {
	    ncrlf();
	    sout("CANCELLED");
	    ncrlf();
	    break;
	} else if (key == 8) {
	    do
		if (ep != s) {
		    cout('\b');
		    --ep;
		}
	    while (--n > 0);
	} else if (key == 127) {
	    do
		if (ep != s) {
		    --ep;
		    cout(*ep);
		}
	    while (--n > 0);
	} else if ((key == 'i') || (key == 'I')) {
insert:
	    FOREVER {
		key = get_key();
		if (key == 0x1b)
		    break;
		else if (key == '\r')
		    goto end_edit;
		else if (key >= ' ') {
		    cout(key);
		    t = ep;
		    while (*t)
			++t;
		    *(t+1) = '\0';
		    --t;
		    while (t >= ep) {
			*(t+1) = *t;
			--t;
		    }
		    *ep++ = key;
		}
		    else cout(7);
	    }
	} else if ((key == 'd') || (key == 'D')) {
	    if (*ep) {
		f = YES;
		cout('\\');
	    } else
		f = NO;
	    do
		if (*ep) {
		    cout(*ep);
		    t = ep;
		    while (*t) {
			*t = *(t+1);
			++t;
		    }
		}
	    while (--n > 0);
	    if (f)
		cout('\\');
	} else if ((key == 'l') || (key == 'L')) {
	    ncrlf();
	    goto retype;
	} else if ((key == 's') || (key == 'S')) {
	    key = get_key();
	    do {
		if (*ep) {
		    cout(*ep++);
		    while (*ep && (*ep != key))
			cout(*ep++);
		}
		--n;
	    } while (n > 0);
	} else cout(7);
    }
    cook_keyboard();
}

/* C string from basic string */

static char *cstring(basic_string s) {
    s.storage[s.length] = '\0';
    return s.storage;
}

/* Save program (binary form) */

static void save(char *filename) {
    FILE *f;
    unsigned char *end;

    f = fopen(filename, "w");
    if (f == NULL)
	return;
    end = eop();
    fwrite(program, 1, end - program, f);
    fclose(f);
}

/* Save program in ASCII format */

static void save_ascii(char *filename) {
    FILE *f;
    unsigned char *p;

    f = fopen(filename, "w");
    for (p = sop(); *p != EOP; p = skip(p)) {
	list_line(p, src);
	fprintf(f, "%s\n", src);
    }
    fclose(f);
}

/* Merge ascii program */

static void merge(char *filename) {
    FILE *f;
    int n;

    f = fopen(filename, "r");
    if (f == NULL)
	return;
    while (fgets(src, sizeof(src), f) != NULL) {
	n = strlen(src);
	if (n > 0)
	    if (src[n - 1] == '\n')
		src[n - 1] = '\0';
	tokenize_line(src, tkn_buf);
	if (tkn_buf[0] == CONSTANT)
	    edit_program(tkn_buf);
	else {
	    noedit = YES;
	    edit_program(tkn_buf);
	}
    }
    fclose(f);
}

/* Load binary program */

static void load(char *filename) {
    FILE *f;
    unsigned char z;

    program[0] = START;
    program[1] = EOP;
    noedit = NO;
    f = fopen(filename, "r");
    if (f == NULL)
	return;
    fread(&z, 1, 1, f);
    if (z == START) {
	fread(sop(), 1, MAX_PROGRAM, f);
	fclose(f);
	return;
    }
    fclose(f);
    merge(filename);
}

/* Return true if op is a unary op */

static int is_unary_op(unsigned char op) {
    return (op == UNARY_PLUS) || (op == UNARY_MINUS) || (op == UNARY_NOT);
}

/* Unstack and do unary ops. All unary ops have the same precedence,
 * which is higher than any binary ops.
 */

static void do_unary_ops(int lim) {
    unsigned char op;
    double x;

    FOREVER {
	if (opstack_ptr == lim)
	    return;
	op = opstack[opstack_ptr - 1];
	if (!is_unary_op(op))
	    return;
	--opstack_ptr;
	if (valuestack_ptr == 0)
	    error(ERROR_SYNTAX);
	x = valuestack[--valuestack_ptr];
	if (op == UNARY_PLUS)
	    valuestack[valuestack_ptr++] = x;
	else if (op == UNARY_MINUS)
	    valuestack[valuestack_ptr++] = -x;
	else if (op == UNARY_NOT)
	    valuestack[valuestack_ptr++] = ~d_to_li(x);
    }
}

/* Return precedence of a binary operator. Note that '(' is queued
 * as a binary operator, even though its not. '(' must have the
 * lowest precedence.
 */

static int precedence(unsigned char op) {
    switch (op) {
	case EQUAL: return 1;
	case PLUS: return 2;
	case DASH: return 2;
	case STAR: return 3;
	case SLASH: return 3;
	case HAT: return 4;
	case AND: return 0;
	case OR: return 0;
	case XOR: return 0;
	case IMP: return 0;
	case EQV: return 0;
	case MOD: return 3;
	case NE: return 1;
	case LE: return 1;
	case GE: return 1;
	case LT: return 1;
	case GT: return 1;
	case IDIV: return 3;
	case OPENP: return -1;
    }
    error(ERROR_SYNTAX);
    return -1;
}

/* Round x to places */
	
static double roundx(double x, int places) {
    double r;

    r = 0.5;
    while (places) {
	r = r / 10;
	--places;
    }
    return x + r;
}

/* Return integer part of a double */

static double intx(double x) {
    modf(x, &x);
    return x;
}

/* Execute a binary operator */

static void binary_op(int lim) {
    double x, y;
    unsigned char op;
    long int ix, iy;

    if ((opstack_ptr == lim) || (valuestack_ptr < 2))
	error(ERROR_SYNTAX);
    op = opstack[--opstack_ptr];
    y = valuestack[--valuestack_ptr];
    x = valuestack[--valuestack_ptr];
    ix = d_to_li(x);
    iy = d_to_li(y);
    switch (op) {
	case OPENP: error(ERROR_SYNTAX);
	case PLUS: x = x + y; break;
	case DASH: x = x - y; break;
	case STAR: x = x * y; break;
	case HAT: x = pow(x, y); break;
	case AND: x = ix & iy; break;
	case OR: x = ix | iy; break;
	case XOR: x = ix ^ iy; break;
	case IMP: x = ~ix | (ix & iy); break;
	case EQV: x = ~(ix ^ iy); break;
	case EQUAL: x = (x == y) ? -1 : 0; break;
	case NE: x = (x != y) ? -1 : 0; break;
	case LE: x = (x <= y) ? -1 : 0; break;
	case GE: x = (x >= y) ? -1 : 0; break;
	case LT: x = (x < y) ? -1 : 0; break;
	case GT: x = (x > y) ? -1 : 0; break;
	case SLASH:
	    if (y == 0)
		error(ERROR_MATH);
	    x = x / y;
	    break;
	case MOD:
	    if (y == 0)
		error(ERROR_MATH);
	    x = fmod(x, y);
	    break;
	case IDIV:
	    y = intx(y);
	    if (y == 0)
		error(ERROR_MATH);
	    x = intx(x / y);
	    break;
    }
    valuestack[valuestack_ptr++] = x;
}

/* Unstack binary operators of higher precedence, and do them */

static void do_binary_ops(unsigned char op, int lim) {
    unsigned char op2;

    FOREVER {
	if (opstack_ptr == lim)
	    return;
	op2 = opstack[opstack_ptr - 1];
	if (precedence(op2) >= precedence(op))
	    binary_op(lim);
	else
	    return;
    }
}

/* Skip optional token, return true if token was present */

static int token_optional(unsigned char **p, unsigned char token) {
    if (**p == token) {
	++(*p);
	return YES;
    }
    return NO;
}

/* Token is required */

static void token_needed(unsigned char *p, unsigned char token) {
    if (*p != token)
	error(ERROR_SYNTAX);
}

/* Eat required token */

static void eat_token(unsigned char **p, unsigned char token) {
    token_needed(*p, token);
    ++(*p);
}

/* Close paren required */ 

static void closep(unsigned char **p) {
    eat_token(p, CLOSEP);
}

/* Comma required */

static void comma(unsigned char **p) {
    eat_token(p, COMMA);
}

/* Get value of variable */

static int gexpression(unsigned char **p, double *rn, basic_string *rs);
static double expression(unsigned char **p);
static basic_string sexpression(unsigned char **p);

/* Collect dimensions - dptr has number of dimensions (0..63) */

static unsigned char *get_dimensions(unsigned char *p, int *dptr,
				     double *dimensions) {
    *dptr = 0;
    if (*p == OPENP) {
	if (p[1] == CLOSEP) {
	    /* for COMMON A() */
	    return p+2;
	}
	do {
	    ++p;
	    dimensions[*dptr] = expression(&p);
	    if (dimensions[*dptr] < 0)
		error(ERROR_DIM);
	    ++*dptr;
	} while (*p == COMMA);
	closep(&p);
    }
    return p;
}

/* Must have variable */

static struct variable *must_have_variable(unsigned char *p) {
    struct variable *pv;

    pv = find_variable((char *)p+2);
    if (pv == NULL)
	error(ERROR_NOVAR);
    return pv;
}

/* Compute index, returns pointer to element */

typedef	basic_string bsarray[1];

static void *array_index(unsigned char *p, int *dptr, double *dimensions) {
    struct array *pa;
    struct variable *pv;
    double index, ld;
    int i;
    bsarray *pbs;

    pv = must_have_variable(p);
    if ((pv->type != ARRAY) && (pv->type != STRING_ARRAY))
	error(ERROR_TYPE);
    pa = pv->descriptor;
    if (*dptr != pa->ndims)
	error(ERROR_DIM);
    index = 0;
    ld = 1;
    for (i = 0; i < *dptr; ++i) {
	index *= ld;
	index += dimensions[i];
	if (dimensions[i] > pa->dims[i]) 
	    error(ERROR_DIM);
	ld = pa->dims[i] + 1;
    }
    if (pv->type == ARRAY)
	return (double *)pa->values + (int)index;
    else {
	pbs = (bsarray *)pa->values;
	return pbs + (int)index;
    }
}

/* Return pointer to dimensioned variable element */

static void *dimensioned_variable(unsigned char **t) {
    void *valuep;
    double dimensions[MAX_DIMS];
    int dptr;
    unsigned char *p = *t;

    *t = skip_variable(*t);
    *t = get_dimensions(*t, &dptr, dimensions);
    if (dptr != 0)
	valuep = array_index(p, &dptr, dimensions);
    else
	valuep = NULL;
    return valuep;
}

/* Get variable value */

static double get_variable(unsigned char **t) {
    unsigned char *p = *t;
    double x;
    char *name;
    void *valuep;

    name = variable_name(p);
    valuep = dimensioned_variable(&p);
    if (valuep == NULL)
	x = get_scalar(name);
    else {
	x = *(double *)valuep;
	if (is_integer_variable((unsigned char *)name-2))
	    x = intx(x);
    }
    *t = p;
    return x;
}

/* Get value of string variable */

static basic_string get_string_variable(unsigned char **t) {
    unsigned char *p = *t;
    basic_string s;
    char *name;
    void *valuep;

    name = variable_name(p);
    valuep = dimensioned_variable(&p);
    if (valuep == NULL)
	s = get_string(name);
    else
	s = *(basic_string *)valuep;
    *t = p;
    return s;
}

/* Set a simple variable */

static void set_variable(unsigned char *p, double v) {
    if (is_integer_variable(p))
	v = intx(v);
    set_scalar((char *)p+2, v);
}

/* Set a string variable */

static void set_string_variable(unsigned char *p, basic_string s) {
    set_string((char *)p+2, s);
}

/* Print basic_string */

static void print_basic_string(basic_string s) {
    int i;

    for (i = 0; i < s.length; ++i)
	cout(s.storage[i]);
}

/* Leftmost x characters of string */

static void left(basic_string *s, double x) {
    if (x < s->length)
	s->length = x;
}

/* Rightmost x characters of string */

static void right(basic_string *s, double x) {
    if (x < s->length) {
	s->storage += s->length - (int)x;
	s->length = (int)x;
    }
}

/* Middle of string */

static void mid(basic_string *s, double p, double l) {
    if (p >= s->length)
	s->length = 0;
    else {
	s->storage += (int)p - 1;
	s->length -= (int)p - 1;
    }
    if (l >= 0) {
	if (s->length > l)
	    s->length = l;
    }
}

#define NO_TYPE		0
#define STRING_TYPE	1
#define NUMBER_TYPE	2

/* Make a string */

static basic_string make_string(int n) {
    basic_string s;
    char *p;

    s.length = n;
    p = get_memory_atomic(n + 1);
    s.storage = p;
    return s;
}

/* Single arg math functions */

static double one_arg(unsigned char **p) {
    double x;

    ++(*p);
    x = expression(p);
    closep(p);
    return x;
}

/* Single string arg functions */

static basic_string one_string_arg(unsigned char **p) {
    basic_string s;

    ++(*p);
    s = sexpression(p);
    closep(p);
    return s;
}

/* A basic string term. This can be a string variable, constant, or
 * a function returning a string.
 */

static int string_term(unsigned char **p, basic_string *rs) {
    int tkn;
    basic_string s, s2;
    unsigned char *t;
    double x, x1;
    time_t now;
    struct tm *tm;
    int i, j, k;
    char c;
    unsigned long d;
    int sh, mask, bits, mask1, bits1;
    char *sp;
    short int c_s;
    long int c_i;
    float c_f;
    int lead;

    tkn = **p;
    switch (tkn) {
	case VARIABLE:
	    if (!is_string_variable(*p))
		return NO_TYPE;
	    s = get_string_variable(p);
	    break;
	case STRING:
	    /* Build string descriptor that uses program itself
	     * for storage.
	     */
	    t = *p;
	    ++t;
	    s.length = *t - 1;
	    ++t;
	    s.storage = (char *)t;
	    t += t[-1];	/* XXX */
	    *p = t;
	    break;
	case ENVIRONS:
	    ++(*p);
	    i = gexpression(p, &x1, &s);
	    closep(p);
	    if (i == STRING_TYPE)
		sp = getenv(cstring(s));
	    else { /* numeric 1..n */
		for (j = 0; environ[j]; ++j) {
		    x1 = x1 - 1;
		    if (x1 <= 0)
			break;
		}
		sp = environ[j];
	    }
	    if (sp == NULL)
		sp = "";
	    i = strlen(sp);
	    s = make_string(i);
	    memcpy(s.storage, sp, i);
	    break;
	case LEFT:
	    ++(*p);
	    s = sexpression(p);
	    comma(p);
	    x = expression(p);
	    closep(p);
	    left(&s, x);
	    break;
	case RIGHT:
	    ++(*p);
	    s = sexpression(p);
	    comma(p);
	    x = expression(p);
	    closep(p);
	    right(&s, x);
	    break;
	case MID:
	    ++(*p);
	    s = sexpression(p);
	    comma(p);
	    x = expression(p);
	    if (token_optional(p, COMMA))
		x1 = expression(p);
	    else
		x1 = -1;
	    closep(p);
	    mid(&s, x, x1);
	    break;
	case CHR:
	    x = one_arg(p);
	    s = make_string(1);
	    s.storage[0] = (char)x;
	    break;
	case TIME:
	    ++(*p);
	    time(&now);
	    tm = localtime(&now);
	    s = make_string(9);
	    s.length = 8;
	    sprintf(s.storage, "%02d:%02d:%02d",
			       tm->tm_hour, tm->tm_min, tm->tm_sec);
	    break;
	case DATE:
	    ++(*p);
	    time(&now);
	    tm = localtime(&now);
	    s = make_string(11);
	    s.length = 10;
	    sprintf(s.storage, "%02d-%02d-%04d",
			       tm->tm_mon+1, tm->tm_mday,
			       1900 + tm->tm_year);
	    break;
	case SPACE:	/* SPACE$( */
	    c = ' ';
	    x = one_arg(p);
fill_c:
	    if (x < 0)
		x = 0;
	    s = make_string((int)x);
	    for (i = 0; i < (int)x; ++i)
		s.storage[i] = c;
	    break;
	case LTRIM:
	    s = one_string_arg(p);
	    while ((s.length > 0) &&
		   (s.storage[0] <= ' ')) {
		--s.length;
		++s.storage;
	    }
	    break;
	case RTRIM:
	    s = one_string_arg(p);
	    while ((s.length > 0) &&
		   (s.storage[s.length - 1] <= ' '))
		--s.length;
	    break;
	case LCASE:
	    s2 = one_string_arg(p);
	    s = make_string(s2.length);
	    for (i = 0; i < s2.length; ++i) {
		c = s2.storage[i];
		if (('A' <= c) && (c <= 'Z'))
		    c = c - 'A' + 'a';
		s.storage[i] = c;
	    }
	    break;
	case UCASE:
	    s2 = one_string_arg(p);
	    s = make_string(s2.length);
	    for (i = 0; i < s2.length; ++i) {
		c = s2.storage[i];
		if (('a' <= c) && (c <= 'z'))
		    c = c - 'a' + 'A';
		s.storage[i] = c;
	    }
	    break;
	case STRINGF:
	    ++(*p);
	    x = expression(p);
	    comma(p);
	    i = gexpression(p, &x1, &s);
	    closep(p);
	    if (i == STRING_TYPE) {
		if (s.length == 0)
		    c = 0;
		else
		    c = s.storage[0];
	    } else
		c = (int)x1;
	    goto fill_c;
	case HEX:
	    mask = 0xF;
	    bits = 4;
	    mask1 = 0xF;
	    bits1 = 4;
cvt_base:
	    x = one_arg(p);
	    d = x;
	    s = make_string(32);
	    sp = s.storage;
	    lead = 0;
	    sh = 32;
	    while (sh) {
		sh -= bits1;
		if (lead || ((d >> sh) & mask1)) {
		    *sp++ = h_set[(d >> sh) & mask1];
		    lead = 1;
		}
		bits1 = bits;
		mask1 = mask;
	    }
	    if (lead == 0)
		*sp++ = '0';
		s.length = sp - s.storage;
	    break;
	case OCT:
	    mask = 0x7;
	    bits = 3;
	    mask1 = 0x3;
	    bits1 = 2;
	    goto cvt_base;
	case BIN:
	    mask = 1;
	    bits = 1;
	    mask1 = 1;
	    bits1 = 1;
	    goto cvt_base;
	case STRF:
	    x = one_arg(p);
	    s = make_string(80);
	    s.length = sprintf(s.storage, "%.10G", x);
	    break;
	case MKD:
	    x = one_arg(p);
	    s = make_string(sizeof(double));
	    memcpy(s.storage, &x, sizeof(double));
	    break;
	case MKI:
	    x = one_arg(p);
	    c_s = x;
	    s = make_string(sizeof(short int));
	    memcpy(s.storage, &c_s, sizeof(short int));
	    break;
	case MKS:
	    x = one_arg(p);
	    c_f = x;
	    s = make_string(sizeof(float));
	    memcpy(s.storage, &c_f, sizeof(float));
	    break;
	case MKL:
	    x = one_arg(p);
	    c_i = x;
	    s = make_string(sizeof(long int));
	    memcpy(s.storage, &c_i, sizeof(long int));
	    break;
	case INKEY:
	    ++(*p);
	    s = make_string(3);
	    if (use_curses) {
		noecho();
		cbreak();
		nodelay(sc, TRUE);
		i = getch();
		echo();
		nocbreak();
		nodelay(sc, FALSE);
		if (i == ERR)
		    s.length = 0;
		else {
		    if (i >= 0400) {
			s.storage[0] = (i >> 8);
			s.storage[1] = i;
			s.length = 2;
		    } else {
			s.storage[0] = i;
			s.length = 1;
		    }
		}
	    } else
		s.length = 0;
	    break;
	case INPUTS:
	    /* INPUT$(n[,[#]fn) */
	    ++(*p);
	    x = expression(p);
	    if (token_optional(p, COMMA)) {
		token_optional(p, HASH);
		select_file(expression(p));
	    }
	    s = make_string(x+1);
	    s.length = x;
	    if (outdev < 0) {
		if (use_curses) {
		    noecho();
		    cbreak();
		    if (s.length == 1) {
			k = getch();
			if (k >= 0400) {
			    s.storage[0] = (k >> 8);
			    s.storage[1] = k;
			    s.length = 2;
			} else
			    s.storage[0] = k;
		    } else {
			for (i = 0; i < s.length; ++i)
			    s.storage[i] = getch();
		    }
		    echo();
		    nocbreak();
		} else
		    s.length = 0;
	    } else {
		s.length = fread(s.storage,
				  1,
				  x,
				  pfile(x1));
	    }
	    select_file(CONSOLE);
	    closep(p);
	    break;
	default:
	    return NO_TYPE;
    }
    *rs = s;
    return STRING_TYPE;
}

/* String concatenation.  We keep track of parenthesis nesting */

static int string_term2(unsigned char **p, basic_string *rs, int *nparens) {
    int i;
    char *ns;
    basic_string bs;
    int np = *nparens;

    while (**p == OPENP) {
	++(*p);
	++(*nparens);
	opstack[opstack_ptr++] = OPENP;
    }
    i = string_term(p, rs);
    if (i == NO_TYPE)
	return NO_TYPE;
    while ((np != *nparens) && (**p == CLOSEP)) {
	++(*p);
	--(*nparens);
	if ((opstack_ptr <= 0) || (opstack[--opstack_ptr] != OPENP))
	    error(ERROR_SYNTAX);
	if (*nparens < np)
	    error(ERROR_SYNTAX);
    }
    while (token_optional(p, PLUS)) {
	while (**p == OPENP) {
	    ++(*p);
	    ++(*nparens);
	    opstack[opstack_ptr++] = OPENP;
	}
	i = string_term(p, &bs);
	if (i != STRING_TYPE)
	    error(ERROR_TYPE);
	while ((np != *nparens) && (**p == CLOSEP)) {
	    ++(*p);
	    --(*nparens);
	    if ((opstack_ptr <= 0) ||
		(opstack[--opstack_ptr] != OPENP))
		error(ERROR_SYNTAX);
	    if (*nparens < np)
		error(ERROR_SYNTAX);
	}
	i = rs->length + bs.length;
	ns = get_memory_atomic(i + 1);
	memcpy(ns, rs->storage, rs->length);
	memcpy(ns + rs->length, bs.storage, bs.length);
	bs.length = i;
	bs.storage = ns;
	*rs = bs;
	}
    return STRING_TYPE;
}

/* Compare two basic strings */

static int compare_strings(int op, basic_string b1, basic_string b2) {
    int i, r;

    r = 0;
    for (i = 0; i < b1.length; ++i) {
	if (i > b2.length)
	    break;
	r = b1.storage[i] - b2.storage[i];
	if (r != 0)
	    break;
    }
    if ((r == 0) && (i == b1.length) && (i == b2.length)) {
	/* =, <=, >= */
	if ((op == EQUAL) || (op == LE) || (op == GE))
	    return -1;
	return 0;
    }
    if (op == NE) /* <> */
	return -1;
    if (r <= 0) {
	/* <=, < */
	if ((op == LE) || (op == LT))
	return -1;
    }
    if (r > 0) {
	/* >=, > */
	if ((op == GE) || (op == GT))
	    return -1;
    }
    return 0;
}

/* A string expression. This may be a string, contenation of strings,
 * or a relational operator involving strings. Can return string
 * or number.
 */

static int string_expression(unsigned char **p, double *rn, basic_string *rs,
			     int *nparens) {
    int i, tkn;
    basic_string s;

    i = string_term2(p, rs, nparens);
    if (i == NO_TYPE)
	return i;
    tkn = **p;
    switch (tkn) {
	case EQUAL:	/* = */
	case NE:	/* <> */
	case LE:	/* <= */
	case GE:	/* >= */
	case LT:	/* < */
	case GT:	/* > */
	    ++(*p);
	    i = string_term2(p, &s, nparens);
	    if (i != STRING_TYPE)
		error(ERROR_TYPE);
	    *rn = compare_strings(tkn, *rs, s);
	    return NUMBER_TYPE;

	default:
	    return STRING_TYPE;
    }
}

/* Evaluate expression
 * 
 * Expressions begin with: VARIABLE, CONSTANT, +, -, NOT, (,
 * or a function.
 */

static int gexpression(unsigned char **p, double *rn, basic_string *rs) {
    int tkn;
    int state;
    double x;
    int entry_opstack_ptr = opstack_ptr;
    int entry_valuestack_ptr = valuestack_ptr;
    int nparens, i, j;
    time_t now;
    struct tm *tm;
    basic_string s, s2;
    char *sp;
    float c_f;
    long int c_i;
    short int c_s;
    unsigned char op;
    int base;

    nparens = 0;
    state = 0;

    /* This may be a string expression, so give it a whirl */
    i = string_expression(p, &x, rs, &nparens);
    if (i == NUMBER_TYPE)
	goto stack_unary; /* string comparision */
    else if (i == STRING_TYPE)
	return STRING_TYPE; /* string expression */
    /* else its NO_TYPE, so try it as a numeric expression */
    goto top2;

top:
    i = string_expression(p, &x, rs, &nparens);
    if (i == NUMBER_TYPE)
	goto stack_unary;
    else if (i == STRING_TYPE)
	error(ERROR_TYPE);	/* Must be numeric result */
	/* else NO_TYPE */
top2:
    if (state == 0) {
	tkn = **p;
	switch (tkn) {
	    case FN:
		{
		void *v = variables;
		double y, *py;
		basic_string s, *ps;
		int t;
		unsigned char *fp;
		char *name;

		++(*p);
		fp = fns[(*p)[2]];
		*p = skip_variable(*p);
		token_optional(p, OPENP);
		fp = skip_variable(fp);
		token_optional(&fp, OPENP);
		while (**p != CLOSEP) {
		    token_needed(fp, VARIABLE);
		    name = variable_name(fp);
		    fp = skip_variable(fp);
		    t = gexpression(p, &y, &s);
		    if (t == STRING_TYPE) {
			ps = get_memory(
					sizeof(basic_string));
			*ps = s;
			create_variable(name, ps,
					SIMPLE_STRING);
		    } else {/* t == NUMBER_TYPE */
			py = get_memory_atomic(
						sizeof(double));
			*py = y;
			create_variable(name, py,
					SIMPLE_SCALAR);
		    }
		    if (!token_optional(p, COMMA))
			break;
		    eat_token(&fp, COMMA);
		}
		token_optional(p, CLOSEP);
		token_optional(&fp, CLOSEP);
		eat_token(&fp, EQUAL);
		flush_variable_cache();
		t = gexpression(&fp, &y, &s);
		x = y;
		/* restore variables */
		variables = v;
		flush_variable_cache();
		goto stack_unary;
		}
	    case CSNG:
	    case CDBL:
		x = one_arg(p);
		goto stack_unary;
	    case FREEFILE:
		++(*p);
		x = freefile();
		goto stack_unary;
	    case INSTR:
		/* INSTR([p,],s$,target$) */
		++(*p);
		i = gexpression(p, &x, &s);
		comma(p);
		if (i == NUMBER_TYPE)
		    j = (int)x;
		else
		    j = 1;
		if (i != STRING_TYPE) {
		    s = sexpression(p);
		    comma(p);
		}
		s2 = sexpression(p);
		closep(p);
		x = 0;
		for (--j; j < s.length; ++j) {
		    for (i = 0; i < s2.length; ++i) {
			if ((j + i) >= s.length)
			    goto stack_unary;
			if (s.storage[j + i] !=
			    s2.storage[i])
			    break;
		    }
		    if (i == s2.length) {
			x = j + 1;
			goto stack_unary;
		    }
		}
		goto stack_unary;
	    case ABS:
		x = one_arg(p);
		if (x < 0)
		    x = -x;
		goto stack_unary;
	    case ATN:
		x = one_arg(p);
		x = atan(x);
		goto stack_unary;
	    case COS:
		x = one_arg(p);
		x = cos(x);
		goto stack_unary;
	    case EXP:
		x = one_arg(p);
		x = exp(x);
		goto stack_unary;
	    case INTF:
		x = one_arg(p);
		if (x < 0)
		    x = x - 0.5;
		x = intx(x);
		goto stack_unary;
	    case FIX:
		x = one_arg(p);
		x = intx(x);
		goto stack_unary;
	    case LOG:
		x = one_arg(p);
		x = log(x);
		goto stack_unary;
	    case SGN:
		x = one_arg(p);
		if (x < 0)
		    x = -1;
		if (x > 0)
		    x = 1;
		goto stack_unary;
	    case SIN:
		x = one_arg(p);
		x = sin(x);
		goto stack_unary;
	    case SQR:
		x = one_arg(p);
		x = sqrt(x);
		goto stack_unary;
	    case TAN:
		x = one_arg(p);
		x = tan(x);
		goto stack_unary;
	    case TAB:
		x = one_arg(p);
		tab = x;
		goto stack_unary;
	    case SPC:
		x = one_arg(p);
		tab = pos + x;
		goto stack_unary;
	    case ERL:
		++(*p);
		x = erl;
		goto stack_unary;
	    case ERRF:
		++(*p);
		x = err;
		if (onerror < 0)
		    err = 0;
		goto stack_unary;
	    case TIMER:
		++(*p);
		time(&now);
		tm = localtime(&now);
		x = ((tm->tm_hour * 60 + tm->tm_min) * 60) +
                    tm->tm_sec;
		goto stack_unary;
	    case RND:
		x = one_arg(p);
		x *= (double)rand() / RAND_MAX;
		goto stack_unary;
	    case POS:
		++(*p);
		token_optional(p, HASH);
		x = expression(p);
		closep(p);
		if (use_curses) {
		    int sx, sy;

		    getyx(sc, sy, sx);
		    x = sx;
		} else if (x == 0)
		    x = pos;
		else
		    x = files[(int)x].pos;
		goto stack_unary;
	    case LPOS:
		one_arg(p);
		x = lpos;
		goto stack_unary;
	    case CSRLIN:
		++(*p);
		if (use_curses) {
		    int sx, sy;

		    getyx(sc, sy, sx);
		    x = sy;
		} else
		    x = 0;
		goto stack_unary;
	    case LEN:
		s = one_string_arg(p);
		x = s.length;
		goto stack_unary;
	    case ASC:
		s = one_string_arg(p);
		if (s.length == 0)
		    x = 0;
		else
		    x = s.storage[0];
		goto stack_unary;
	    case VAL:
		s = one_string_arg(p);
		sp = cstring(s);
		x = val(&sp, 1, &base);
		goto stack_unary;
	    case PEEK:
		x = one_arg(p);
		x = peek((size_t)x);
		goto stack_unary;
	    case INP:
		x = one_arg(p);
		x = inp((int)x);
		goto stack_unary;
	    case CINT:
		x = one_arg(p);
		if (x > 0)
		    x += 0.5;
		else if (x < 0)
		    x -= 0.5;
		x = intx(x);
		goto stack_unary;
	    case CVD:
		s = one_string_arg(p);
		memcpy(&x, s.storage, sizeof(double));
		goto stack_unary;
	    case CVI:
		s = one_string_arg(p);
		memcpy(&c_s, s.storage, sizeof(short int));
		x = c_s;
		goto stack_unary;
	    case CVS:
		s = one_string_arg(p);
		memcpy(&c_f, s.storage, sizeof(float));
		x = c_f;
		goto stack_unary;
	    case CVL:
		s = one_string_arg(p);
		memcpy(&c_i, s.storage, sizeof(long int));
		x = c_i;
		    goto stack_unary;
	    case EOFF:
		if (*((*p) + 1) == HASH)
		    ++(*p);
		x = one_arg(p);
		if (feof(pfile(x))) {
		    x = -1;
		    goto stack_unary;
		}
		i = fgetc(pfile(x));
		if (i == EOF) {
		    x = -1;
		    goto stack_unary;
		}
		ungetc(i, pfile(x));
		x = 0;
		goto stack_unary;
	    case LOF:
		{
		long e, n;

		if (*((*p) + 1) == HASH)
		    ++(*p);
		x = one_arg(p);
		n = ftell(pfile(x));
		fseek(pfile(x), 0, SEEK_END);
		e = ftell(pfile(x));
		fseek(pfile(x), n, SEEK_SET);
		x = e;
		}
		goto stack_unary;
	    case LOC:
		if (*((*p) + 1) == HASH)
		    ++(*p);
		x = one_arg(p);
		x = ftell(pfile(x));
		if (files[(int)x].mode == MODE_RANDOM)
		    x = x / files[(int)x].reclen;
		goto stack_unary;
	    case VARPTR:
		{
		/* XXX If only array header is referenced,
		 * return pointer to array header
		 */
		void *valuep;
		char *name;
			       
		++(*p);
		must_have_variable(*p);
		name = variable_name(*p);
		if (is_string_variable(*p)) {
		    valuep = dimensioned_variable(p);
		    if (valuep == NULL)
			valuep = address_string(name);
		} else {
		    valuep = dimensioned_variable(p);
		    if (valuep == NULL)
			valuep = address_scalar(name);
		}

		closep(p);
		x = (double)(unsigned long)valuep;
		goto stack_unary;
		}
	    case VARIABLE:
		/* simple scalar, or array */
		x = get_variable(p);
stack_unary:
		valuestack[valuestack_ptr++] = x;
		do_unary_ops(entry_opstack_ptr);
		state = 1;
		goto top;
	    case HEX_MODE:
	    case OCTAL_MODE:
	    case BINARY_MODE:
		++(*p);
		/* next must be CONSTANT */
	    case CONSTANT:
		++(*p);
		x = R(*p);
		*p += sizeof(double);
		goto stack_unary;
	    case PLUS:
		++(*p);
		opstack[opstack_ptr++] = UNARY_PLUS;
		goto top;
	    case DASH:
		++(*p);
		opstack[opstack_ptr++] = UNARY_MINUS;
		goto top;
	    case NOT:
		++(*p);
		opstack[opstack_ptr++] = UNARY_NOT;
		goto top;
	    case OPENP:
		++nparens;
		++(*p);
		opstack[opstack_ptr++] = OPENP;
		goto top;
	    default:
		error(ERROR_SYNTAX);
	}
    } else {
	tkn = **p;
	switch (tkn) {
	    case CLOSEP:	/* ) */
		if (nparens == 0)
		    goto ret_x;
		--nparens;
		++(*p);
		while ((opstack_ptr > entry_opstack_ptr) &&
		       (opstack[opstack_ptr-1] != OPENP)) {
		    op = opstack[opstack_ptr - 1];
		    if (is_unary_op(op))
			do_unary_ops(entry_opstack_ptr);
		    else
			binary_op(entry_opstack_ptr);
		}
		if (opstack_ptr == entry_opstack_ptr)
		    error(ERROR_SYNTAX);
		--opstack_ptr;
		goto top;
	    case EQUAL:
	    case PLUS:
	    case DASH:
	    case STAR:
	    case SLASH:
	    case HAT:
	    case AND:
	    case OR:
	    case XOR:
	    case IMP:
	    case EQV:
	    case MOD:
	    case NE:
	    case LE:
	    case GE:
	    case LT:
	    case GT:
	    case IDIV:
		do_unary_ops(entry_opstack_ptr);
		do_binary_ops(tkn, entry_opstack_ptr);
		state = 0;
		++(*p);
		opstack[opstack_ptr++] = tkn;
		goto top;
	    default:
ret_x:
		while (opstack_ptr > entry_opstack_ptr) {
		    op = opstack[opstack_ptr - 1];
		    if (is_unary_op(op))
			do_unary_ops(entry_opstack_ptr);
		    else
			binary_op(entry_opstack_ptr);
		}
		x = valuestack[--valuestack_ptr];
		if ((opstack_ptr != entry_opstack_ptr) ||
		    (valuestack_ptr != entry_valuestack_ptr))
		    error(ERROR_SYNTAX);
		*rn = x;
		return NUMBER_TYPE;
	    }
	}
    error(ERROR_SYNTAX);
    return NUMBER_TYPE;
}

/* Evaluate a numeric expression */

static double expression(unsigned char **p) {
    double x;
    basic_string s;

    if (gexpression(p, &x, &s) != NUMBER_TYPE)
	error(ERROR_TYPE);
    return x;
}

/* Evaluate a string expression */

static basic_string sexpression(unsigned char **p) {
    double x;
    basic_string s;

    if (gexpression(p, &x, &s) != STRING_TYPE)
	error(ERROR_TYPE);
    return s;
}

/* Vars */

static void vars(void) {
    struct variable *v;
    char buf[256];
    char *s;
    char *types[] = { "NAME ONLY", "NUMBER", "STRING", "NUMERIC ARRAY",
		      "STRING ARRAY" };
    int i;
    struct array *a;

    for (v = variables; v; v = v->next) {
	s = buf;
	s += sprintf(s, "%s %s", v->name, types[v->type]);
	if (v->common)
	    s += sprintf(s, " COMMON");
	if ((v->type == STRING_ARRAY) || (v->type == ARRAY)) {
	    s += sprintf(s, " (");
	    a = v->descriptor;
	    for (i = 0; i < a->ndims - 1; ++i) {
		s += sprintf(s, "%g,", a->dims[i]);
	    }
	    s += sprintf(s, "%g)", a->dims[i]);
	} else if (v->type == SIMPLE_SCALAR)
	    s += sprintf(s, " = %.10G", *(double *)v->descriptor);
	/* else its SIMPLE_STRING */
	sout(buf);
	crlf();
    }
}

/* L/Rset string into variable */

static void lr_set_string(unsigned char *v, basic_string s, int to_right) {
    struct variable *pv;
    basic_string target;
    int i, j;

    pv = must_have_variable(v);
    if (pv->type != SIMPLE_STRING)
	error(ERROR_TYPE);
    target = *(basic_string *)(pv->descriptor);
    if (to_right == 0) {
	for (i = 0; i < target.length; ++i)
	    if (i < s.length)
		target.storage[i] = s.storage[i];
	    else
		target.storage[i] = ' ';
    } else {
	j = s.length - 1;
	for (i = target.length - 1; i != -1; --i)
	    if (j != -1)
		target.storage[i] = s.storage[j--];
	    else
		target.storage[i] = ' ';
    }
}

/* Dimension a variable */

static unsigned char *dim(unsigned char *p) {
    char *name;
    int t;
    int dptr;
    double dimensions[MAX_DIMS];

    token_needed(p, VARIABLE);
    if (is_string_variable(p))
	t = STRING_ARRAY;
    else
	t = ARRAY;
    name = variable_name(p);
    p = skip_variable(p);
    token_needed(p, OPENP);
    p = get_dimensions(p, &dptr, dimensions);
    create_array(name, t, dptr, dimensions);
    return p;
}

/* Make a variable COMMON */

static unsigned char *common(unsigned char *p) {
    struct variable *pv;
    int dptr;
    double dimensions[MAX_DIMS];

    token_needed(p, VARIABLE);
    if (find_variable((char *)p+2) == NULL)
	create_variable((char *)p+2, NULL, NAME_ONLY);
    pv = must_have_variable(p);
    p = skip_variable(p);
    p = get_dimensions(p, &dptr, dimensions);
    pv->common = YES;
    return p;
}

/* Clear a variable */

static void clear_variable(struct variable *pv) {
    destroy_variable(pv);
}

/* Erase an array */

static unsigned char *ferase(unsigned char *p) {
    struct variable *pv;
    char *name;

    token_needed(p, VARIABLE);
    name = variable_name(p);
    p = skip_variable(p);
    pv = find_variable(name);
    if (pv != NULL)
	clear_variable(pv);
    return p;
}

/* Output digits for print_using */

static double digs(double r, int cnt) {
    while (cnt > 0) {
	r = r * 10;
	cout((int)r + '0');
	r = r - intx(r);
	--cnt;
    }
    return r;
}

/* Handle PRINT USING */

static unsigned char *print_using(unsigned char *p) {
    double x, r;
    basic_string bs;
    char *format;
    char *f;
    int i, n, hd;
    int lcnt, rcnt, in_number;
    int leading_sign, trailing_minus, dollar;
    int asterisk, comma, exponent;
    int minus, exp, needed;
    char buf[20];
    double w;

    bs = sexpression(&p);
    f = cstring(bs);
    format = get_memory_atomic(strlen(f) + 1);
    strcpy(format, f);
    eat_token(&p, SEMI);
    f = format;
    FOREVER {
	minus = 0;
	lcnt = 0;
	rcnt = 0;
	in_number = 0;
	leading_sign = 0;
	trailing_minus = 0;
	dollar = 0;
	asterisk = 0;
	comma = 0;
	exponent = 0;

	if (*f == '\0')
	    break;

	if (*f == '_') {
	    ++f;
	    if (*f)
		cout(*f);
	    else
		cout('_');
	    continue;
	}

	if ((*f == '&') || (*f == '\\') || (*f == '!')) {
	    bs = sexpression(&p);
	    token_optional(&p, COMMA);
	}

	if (*f == '&') {
	    /* print complete string */
	    ++f;
	    print_basic_string(bs);
	    continue;
	}

	if (*f == '\\') {
	    /* print n char string */
	    ++f;
	    n = 1;
	    while (*f && (*f != '\\')) {
		++f;
		++n;
	    }
	    if (*f == '\\') {
		++f;
		++n;
	    }
	    for (i = 0; i < n; ++i)
		if (i < bs.length)
		    cout(bs.storage[i]);
		else 
		    cout(' ');
		continue;
	    }
	
	    if (*f == '!') {
		++f;
		/* print one char string */
		if (bs.length >= 1)
		    cout(bs.storage[0]);
		continue;
	    }

	    if (*f == '+') {
		++f;
		leading_sign = 1;
		in_number = 1;
	    }

	    if (*f == '$') {
		if (f[1] && (f[1] == '$'))
		    dollar = 1;
		f += 2;
		++lcnt;
		in_number = 1;
	    } else if (*f == '*') {
		if (f[1] && (f[1] == '*'))
		    asterisk = 1;
		f += 2;
		lcnt += 2;
		in_number = 1;
	    }

	    if ((*f == '#') || (*f == '.'))
		in_number = 1;

	    if (!in_number) {
		/* print *f */
		cout(*f);
		++f;
		continue;
	    }

	    x = expression(&p);
	    token_optional(&p, COMMA);

	    /* Its a number */
	    FOREVER {
		if ((*f == '#') || (*f == ',')) {
		    if (*f == ',')
			comma = 1;
		    ++f;
		    ++lcnt;
		} else
		    break;
	    }
	    if (*f == '.') {
		++f;
		while (*f == '#') {
		    ++rcnt;
		    ++f;
		}
	    }

	    /* Exponent format E+300 -- 5 characters wide */
	    if ((*f == '^') && f[1] && f[2] && f[3] && f[4] &&
		(f[1] == '^') &&
		(f[2] == '^') &&
		(f[3] == '^') &&
		(f[4] == '^')) {
		exponent = 1;
		f += 5;
	    }

	    if (*f == '-') {
		trailing_minus = 1;
		++f;
	    }

	    /* Display number, according to the format */

	    if (x < 0) {
		minus = 1;
		x = -x;
	    }

	    exp = (int)log10(x);
	    if (exp >= 1)
		++exp;
	    if (exp == 0)
		if (x >= 1.0)
		    exp = 1;

	    if (exponent) {
		r = x / pow(10, exp);
		if (r >= 1.0) {
		    r = r / 10.0;
		    ++exp;
		}
		if (leading_sign || trailing_minus)
		    ;
		else {
		    if (minus)
			--lcnt;
		}
		if ((lcnt < 0) || ((lcnt + rcnt) == 0)) {
		    cout('%');
		    lcnt = 1;
		}
		if (minus && (trailing_minus == 0))
		    cout('-');
		if ((minus == 0) && leading_sign)
		    cout('+');
		r = roundx(r, lcnt + rcnt);
		if (lcnt > 0) {
		    exp -= lcnt;
		    r = digs(r, lcnt);
		}
		if (rcnt) {
		    cout('.');
		    digs(r, rcnt);
		}
		cout('E');
		if (exp < 0) {
		    cout('-');
		    exp = -exp;
		} else
		    cout('+');
		n = exp;
		hd = 0;
		if (exp >= 100) {
		    i = exp / 100;
		    cout(i + '0');
		    exp -= i * 100;
		    hd = 1;
		}
		if (hd || (exp >= 10)) {
		    i = exp / 10;
		    cout(i + '0');
		    exp -= i * 10;
		}
		cout(exp + '0');
		if (n < 100)
		    cout(' ');
		if (n < 10)
		    cout(' ');
		} else {
		    r = roundx(x, rcnt);
		    needed = exp;
		    if (comma)
			needed += (exp/3);
		    if (leading_sign || trailing_minus)
			;
		    else {
			if (minus)
			    ++needed;
		    }
		    if ((needed > lcnt) || (abs(exp) > 10)) {
			cout('%');
			if (minus)
			    cout('-');
			sprintf(buf, "%.10G", x);
			sout(buf);
			continue;
		    }
		    for (n = lcnt - needed; n > 0; --n)
			if (asterisk)
			    cout('*');
			else
			    cout(' ');
		    if (minus && (trailing_minus == 0))
			cout('-');
		    if ((minus == 0) && leading_sign)
			cout('+');
		    if (dollar)
			cout('$');
		    /* Now lay down digits, period and commas */
		    /* Note that we cannot have more than 10 digits
		     * on the left. 9,999,999,999
		     */
		    hd = 0;
		    n = 2;
		    for (w = 1000000000; w >= 0.9; w /= 10) {
			i = r / w;
			if (i || hd) {
			    if ((n == 1) && comma)
				cout(',');
			    cout(t_set[i]);
			    hd = 1;
			}
			r = r - (w * i);
			if (--n == 0)
			    n = 3;
		    }
		    if (rcnt) {
			cout('.');
			digs(r, rcnt);
		    }
		}
	    if (minus && trailing_minus)
		cout('-');
	}
    free_memory(format);
    return p;
}

/* XYZZY Handle PRINT */

static unsigned char *print(unsigned char *t) {
    int i;
    double x;
    basic_string s;
    char buf[20];

    do {
#if 0
	if ((*t == COMMA) || (*t == SEMI))
	    ++t;
#endif
	/* ',' advances to next print zone - 14 characters */
	while ((*t == COMMA) || (*t == SEMI)) {
	    if (*t == COMMA) {
		++t;
		do
		    cout(' ');
		while ((*ppos % 14) != 0);
	    } else if (*t == SEMI)
		++t;
	}
	if (statement_end(t))
	    break;
	tab = 0;
	i = gexpression(&t, &x, &s);
	if (tab > 0) {
	    while (*ppos < tab)
		cout(' ');
	} else {
	    if (i == STRING_TYPE)
		print_basic_string(s);
	    else {
		if (x >= 0)
		    cout(' ');
		sprintf(buf, "%.10G ", x);
		sout(buf);
	    }
	    /* ',' advances to next print zone - 14 characters */
#if 0
	    if (*t == COMMA)
		do
		    cout(' ');
		while ((*ppos % 14) != 0);
#endif
	}
    }
    while ((*t == COMMA) || (*t == SEMI))
	NOTHING;
    return t;
}

/* Store number into simple variable, or array */

static void store_number(unsigned char *p, void *valuep, double x) {
    if (valuep)
	*(double *)valuep = x;
    else
	set_variable(p, x);
}

/* Store string into simple variable, or array */

static void store_string(unsigned char *p, void *valuep, basic_string s) {
    void *buf;

    if (valuep) {
	buf = get_memory_atomic(s.length + 1);
	memcpy(buf, s.storage, s.length);
	((basic_string *)valuep)->storage = buf;
	((basic_string *)valuep)->length = s.length;
    } else
	set_string_variable(p, s);
}

/* Create default variables */

static void default_variables(void) {
    double dimensions[1];
    int i;
    struct variable *pv;
    struct array *pa;
    bsarray *pbs;
    basic_string s;

    /* FBASIC = -1 */
    set_scalar("FBASIC", -1);

    /* Command line arguments: ARGS and ARG$(0..ARGS) */
    dimensions[0] = 50;
    create_array("ARG$", STRING_ARRAY, 1, dimensions);
    set_scalar("ARGS", (g_ac <= 2) ? 0 : (g_ac - 2));
    pv = find_variable("ARG$");
    pa = pv->descriptor;
    pbs = (bsarray *)pa->values;
    for (i = 1; i < g_ac; ++i) {
	s.length = strlen(g_av[i]);
	s.storage = g_av[i];
	store_string(NULL, pbs + (i - 1), s);
    }

    /* Make default variables COMMON */
    find_variable("FBASIC")->common = YES;
    find_variable("ARGS")->common = YES;
    find_variable("ARG$")->common = YES;
}

/* Clear */

static void clear_variable(struct variable *pv);

static void fclear(void) {
    int i;

    while (variables)
	clear_variable(variables);
    for (i = 0; i < 256; ++i)
	vtype[i] = 0;
    default_variables();
}

/* String variable required */

static void need_string_variable(unsigned char *t) {
    token_needed(t, VARIABLE);
    if (!is_string_variable(t))
	error(ERROR_TYPE);
}

/* Handle LINE INPUT */

static unsigned char *line_input(unsigned char *t) {
    int suppress_nl = NO;
    char buf[256];
    unsigned char *p;
    void *valuep;
    int i;
    basic_string s;

    if (outdev < 0) {
	if (token_optional(&t, SEMI)) {
	    /* suppress newline on Enter */
	    suppress_nl = YES;
	}
	if (*t == STRING) {
	    sout(variable_name(t));
	    t = skip_variable(t);
	    if (*t == SEMI)
		; /* sout("? "); */
	    else if (*t == COMMA)
		;
	    else
		error(ERROR_SYNTAX);
	    ++t;
	} else {
	    sout("? ");
	}
    }
    need_string_variable(t);
    /* XXX We can simplify this -- address_scalar and
     * address_string will furnish the address of the simple
     * variable. This means that the entire thing can be wrapped up.
     */
    p = t;
    valuep = dimensioned_variable(&t);
    get_line(buf, sizeof(buf));
    i = strlen(buf);
    while ((i >= 0) && ((buf[i] == '\0') ||
			(buf[i] == '\r') ||
			(buf[i] == '\n')))
	--i;
    ++i;
    s = make_string(i);
    memcpy(s.storage, buf, i);
    s.length = i;
    store_string(p, valuep, s);
    return t;
}

/* Handle INPUT */

static unsigned char *input(unsigned char *t) {
    char *prompt = NULL;
    int add_ques = NO;
    int suppress_nl = NO;
    char buf[256];
    char *sp, *sp2;
    unsigned char *p, *p2;
    void *valuep;
    int i, n;
    basic_string s;
    double x;
    int base;

    if (outdev < 0) {
	if (token_optional(&t, SEMI)) {
	    /* suppress newline on Enter */
	    suppress_nl = YES;
	}
	if (*t == STRING) {
	    prompt = variable_name(t);
	    t = skip_variable(t);
	    if (*t == SEMI)
		add_ques = YES;
	    else if (*t == COMMA)
		add_ques = NO;
	    else
		error(ERROR_SYNTAX);
	    ++t;
	} else {
	    prompt = "";
	    add_ques = YES;
	}
    }
    --t;
    p2 = t;
redo:
    t = p2;
    if (outdev < 0) {
	sout(prompt);
	if (add_ques)
	    sout("? ");
    }
    get_line(buf, sizeof(buf));
    sp = buf;
    do {
	++t;
	token_needed(t, VARIABLE);
	p = t;
	valuep = dimensioned_variable(&t);
	if (*sp == '\0') {
redomsg:
	    if (outdev < 0)
		error(ERROR_IO);
	    sout("REDO FROM START");
	    crlf();
	    goto redo;
	}
	while (*sp && (*sp <= ' '))
	    ++sp;
	if (is_string_variable(p)) {
	    /* Get string */
	    i = (*sp == '\"');
	    if (i)
		++sp;
	    n = 0;
	    while (*sp) {
		if (i && (*sp == '\"')) {
		    ++sp;
		    break;
		}
		if (!i && (*sp == ',')) {
		    ++sp;
		    break;
		}
		buf[n++] = *sp++;
	    }
	    if (!i)
		while (n && (buf[n-1] <= ' '))
		    --n;
	    s = make_string(n);
	    memcpy(s.storage, buf, n);
	    s.length = n;
	    store_string(p, valuep, s);
	} else {
	    /* Get number */
	    sp2 = sp;
	    x = val(&sp, 0, &base);
	    if (sp == sp2)
		goto redomsg;
	    store_number(p, valuep, x);
	}
	while (*sp && (*sp <= ' '))
	    ++sp;
	if (*sp == ',')
	    ++sp;
    }
    while (*t == COMMA);
    return t;
}

/* Handle WRITE */

static unsigned char *handle_write(unsigned char *t) {
    int i;
    double x;
    basic_string s;
    char buf[20];

    do {
	if ((*t == COMMA) || (*t == SEMI))
	    ++t;
	if (statement_end(t))
	    break;
	i = gexpression(&t, &x, &s);
	if (i == STRING_TYPE) {
	    cout('\"');
	    print_basic_string(s);
	    cout('\"');
	} else {
	    sprintf(buf, "%.10G", x);
	    sout(buf);
	}
	if (*t == COMMA)
	    cout(',');
    } while ((*t == COMMA) || (*t == SEMI));
    if (t[-1] != SEMI)
	crlf();
    return t;
}

/* Handle READ */

static unsigned char *handle_read(unsigned char *t) {
    unsigned char *p;
    void *valuep;

    token_needed(t, VARIABLE);
    p = t;
    valuep = dimensioned_variable(&t);
    if (token_optional(&data, COMMA))
	;
    else {
	data = skip_data(data);
	if (*data == EOP)
	    error(ERROR_DATA);
	++data;
    }
    if (is_string_variable(p))
	store_string(p, valuep, sexpression(&data));
    else
	store_number(p, valuep, expression(&data));
    return t;
}

/* Handle DEF type */

static unsigned char *handle_deftype(char type, unsigned char *t) {
    char cl, ch;

    ch = 0;
    token_needed(t, VARIABLE);
    cl = *variable_name(t);
    t = skip_variable(t);
    if (token_optional(&t, DASH)) {
	token_needed(t, VARIABLE);
	ch = *variable_name(t);
	t = skip_variable(t);
    }
    vtype[(unsigned char)cl] = type;
    while (cl <= ch)
	vtype[(unsigned char)cl++] = type;
    return t;
}

/* Exit curses */

static void exit_cursesx(void) {
    if (use_curses)
	endwin();
    use_curses = NO;
}

/* Enter curses */

static void enter_curses(void) {
    int n, f, b;

    if (use_curses)
	return;
    sc = initscr();
    start_color();
    idlok(sc, TRUE);
    scrollok(sc, TRUE);
    nonl();
    curs_set(1);
    typeahead(-1);
    echo();
    nocbreak();
    nodelay(sc, FALSE);
    keypad(sc, TRUE);
    use_curses = YES;
    erase();
    move(1, 1);
    for (b = 0; b < 8; ++b)
	for (f = 0; f < 8; ++f) {
	    n = (b << 3) + f;
	    init_pair(n, f, b);
	}
    refresh();
}

/* Delete a range of lines */

static unsigned char *delete_range(unsigned char *t) {
    unsigned char *p;
    double x1, x2, x;

    t = two_ln(t, &x1, &x2);
    if (x1 == DEFAULT_LN)
	error(ERROR_SYNTAX);
    if (x2 == DEFAULT_LN)
	x2 = x1;
    for (p = sop(); *p != EOP; ) {
	if (*p == CONSTANT)
	    x = R(p + 1);
	else
	    x = 0;
	if ((x1 <= x) && (x <= x2))
	    delete_line(find_line(x, YES));
	else
	    p = skip(p);
    }
    return t;
}

/* Renumber program */

static void do_renum(double start, double inc) {
    unsigned char *p;
    int nfixups, fixup_limit; 
    unsigned char **fixups;
    int i;
    double old;

    fixup_limit = 0;
    fixups = NULL;
    nfixups = 0;
    for (p = sop(); *p != EOP; p = skip_one(p)) {
	if ((*p == GOTO) || (*p == GOSUB) || (*p == THEN) ||
	    (*p == ELSE) || (*p == RESUME) || (*p == RESTORE)) {
	    while (p[1] == CONSTANT) {
		if (nfixups >= fixup_limit) {
		    fixup_limit += 1024;
		    fixups = increase_memory(
			fixups,
			fixup_limit *
			    sizeof(unsigned char *));
		}	
		fixups[nfixups++] = p+2;
		p += 2 + sizeof(double);
		if (*p != COMMA)
		    break;
	    }
	}
    }
    for (p = sop(); *p != EOP; p = skip(p)) {
	if (*p == CONSTANT) {
	    old = R(p+1);
	    W(p+1, start);
	    for (i = 0; i < nfixups; ++i)
		if (fixups[i] && (R(fixups[i]) == old)) {
		    W(fixups[i], start);
		    fixups[i] = NULL;
		}
	}
	start += inc;
    }
    free_memory(fixups);
}

/* Generate line numbers for program entry */

static void do_auto(double start, double inc) {
    unsigned char *p;
    int have_line;

    if (noedit)
	error(ERROR_NOEDIT);
    FOREVER {
	have_line = NO;
	p = find_line(start, YES);
	if (*p == CONSTANT)
	    if (R(p+1) == start)
		have_line = YES;
	sprintf(src, "%8g%c", start, have_line ? '*' : ' ');
	sout(src);
	flush_out();
	sprintf(src, "%8g ", start);
	get_line(src+9, sizeof(src)-9);
	/* get_line() returns trailing newline */
	if (strlen(src) > 10) {
	    tokenize_line(src, tkn_buf);
	    edit_program(tkn_buf);
	}
	start += inc;
    }
}

/* Execute. We are entered with a pointer to a token buffer, which
 * we execute until EOP or an editing command. If one of the commands
 * is GOSUB, GOTO, RUN which transfer control to a numbered line,
 * we will begin executing from program space.
 */

static void execute(unsigned char *t) {
    int tkn;
    double x, x1, x2;
    unsigned char *p, *p2;
    int i, in_if;
    basic_string s, s2;
    void *valuep, *valuep2;
    char *sp;
    int *pi;

    in_if = 0;
    FOREVER {
	check_break();
	tkn = *t;
	if (!inerror && running)
	    current_loc = t;
	if (ontimerflag) {
	    time_t last_time;
	    unsigned char *g;

	    last_time = current_time;
	    time(&current_time);
	    secondsleft -= (current_time - last_time);
	    if (secondsleft <= 0) {
		secondsleft = ontimevalue;
		if (gosubstack_ptr >= GOSUB_MAX)
		    error(ERROR_GOSUB);
		gosubstack[gosubstack_ptr++] = t;
		g = ontimer;
		x = expression(&g);
		t = find_line_strict(x);
		continue;
	    }
	}
	if (token_optional(&t, CONSTANT)) {
	    current_line = R(t);
	    t += sizeof(double);
	    tkn = *t;
	    if (ftrace) {
		sprintf(src, "[%.10G]", current_line);
		sout(src);
		flush_out();
	    }
	}
	switch (tkn) {
	case EOL:
	case EOP:
	case COLON:
	case ELSE:
	    break;
	case CONT:
	    t = current_loc;
	    if (t == NULL)
		return;
	    if (*t == STOP)
		++t;
	    else if (*t == CONSTANT) {
		/* line_number STOP */
		if (t[1+sizeof(double)] == STOP)
		    t += 2+sizeof(double);
	    }
	    running = YES;
	    continue;
	case LLIST:
	    select_file(PRINTER);
	    /* drop through to LIST */
	case LIST:	/* LIST [x1][-x2] */
	    ++t;
	    t = two_ln(t, &x1, &x2);
	    if (x1 == DEFAULT_LN) {
		x1 = LOW_LN;
		x2 = HIGH_LN;
	    }
	    if (x2 == DEFAULT_LN)
		x2 = x1;
	    for (p = sop(); *p != EOP; ) {
		if (*p == CONSTANT)
		    x = R(p + 1);
		else
		    x = x1;
		if ((x1 <= x) && (x <= x2)) {
		    list_line(p, src);
		    sout(src);
		    crlf();
		}
		p = skip(p);
	    }
	    break;
	case DELETE:	/* DELETE x1[-x2] */
	    if (noedit)
		error(ERROR_NOEDIT);
	    ++t;
	    t = delete_range(t);
	    clear_execution_state();
	    return;
	case EDIT:
	    ++t;
	    t = two_ln(t, &x1, &x2);
	    if (x1 == DEFAULT_LN) {
		int k = use_curses;
		char buf[256], *sp;
		char fn[256];

		exit_cursesx();
		strcpy(fn, tmpnam(NULL));
		strcat(fn, ".bas");
		save_ascii(fn);
		sp = getenv("FBASIC_EDIT");
		if (sp == NULL)
		    sp = "vi";
		strcpy(buf, sp);
		strcat(buf, " ");
		strcat(buf, fn);
		system(buf);
		load(fn);
		unlink(fn);
		if (k)
		    enter_curses();
		return;
	    }
	    if (noedit)
		error(ERROR_NOEDIT);
	    p = find_line_strict(x1);
	    edit(p);
	    clear_execution_state();
	    return;
	case NEW:
	    program[0] = START;
	    program[1] = EOP;
	    noedit = NO;
	    t = sop();
	    clear_execution_state();
	    return;
	case FREE:
	    ++t;
	    x = eop() - program;
	    garbage_collect();
	    sprintf(src,
	  "Program size %.10G, Free %.10G, Heap size %d",
		x, MAX_PROGRAM - x,
		heap_size());
	    sout(src);
	    crlf();
	    break;
	case SAVE:
	    /* SAVE "filename" [,A] */
	    ++t;
	    s = sexpression(&t);
	    clear_execution_state();
	    if (token_optional(&t, COMMA))
		save_ascii(cstring(s));
	    else
		save(cstring(s));
	    return;
	case LOAD:
	    ++t;
	    s = sexpression(&t);
	    clear_execution_state();
	    load(cstring(s));
	    return;
	case MERGE:
	    ++t;
	    s = sexpression(&t);
	    clear_execution_state();
	    merge(cstring(s));
	    return;
	case TROFF:
	    ++t;
	    ftrace = 0;
	    break;
	case TRON:
	    ++t;
	    ftrace = 1;
	    break;
	case SYSTEM:
	    ++t;
	    if (!statement_end(t)) {
		s = sexpression(&t);
		external_execute(cstring(s), NULL);
	    } else {
		exit_cursesx();
		exit(0);
	    }
	    break;
	case DATA:
	    t = skip_statement(t) - 1;
	    break;
	case OPTION:
	    t = skip_statement(t) - 1;
	    break;
	case REM:
	    t = skip(t) - 1;
	    break;
	case STOP:
	    erl = current_line;
	    /* Fall through to END */
	case END:
	    return;
	case LET:
	    ++t;
	    if (*t == MID)
		goto assign_mid;
	    /* Fall through to VARIABLE */
	case VARIABLE:	/* VARIABLE = expression */
	    p = t;
	    valuep = dimensioned_variable(&t);
	    eat_token(&t, EQUAL);
	    if (is_string_variable(p)) {
		s = sexpression(&t);
		store_string(p, valuep, s);
	    } else {
		x = expression(&t);
		store_number(p, valuep, x);
	    }
	    break;
	case RUN:
	    ++t;
	    t = two_ln(t, &x1, &x2);
	    if (x1 == DEFAULT_LN)
		t = sop();
	    else
		t = find_line_strict(x1);
	    clear_execution_state();
	    in_if = 0;
	    running = YES;
	    continue;
	case GOSUB:
	    if (gosubstack_ptr >= GOSUB_MAX)
		error(ERROR_GOSUB);
	    gosubstack[gosubstack_ptr++] =
		skip_statement(t);
	    /* Drop through to GOTO */
	case GOTO:
	    ++t;
bgoto:	    x = expression(&t);
	    t = find_line_strict(x);
	    in_if = 0;
	    running = YES;
	    continue;
	case RETURN:
	    if (gosubstack_ptr == 0)
		error(ERROR_GOSUB);
	    t = gosubstack[--gosubstack_ptr];
	    in_if = 0;
	    running = YES;
	    continue;
	case IF:
	    ++t;
	    x = expression(&t);
	    eat_token(&t, THEN);
	    if (x) {
		if (*t == CONSTANT)
		    goto bgoto;
		    in_if = 1;
		    continue;
	    }
	    t = skip_else(t);
	    if (*t == CONSTANT)
		goto bgoto;
	    continue;
	case READ:
	    do
		t = handle_read(++t);
	    while (*t == COMMA);
	    break;
	case WHILE:
	    for (i = 0; i < whilestack_ptr; ++i)
		if (whilestack[i] == t) {
		    whilestack_ptr = i;
		    break;
		}
	    if (whilestack_ptr == WHILE_MAX)
		error(ERROR_WHILE);
	    whilestack[whilestack_ptr++] = t;
	    ++t;
	    x = expression(&t);
	    if (x)
		NOTHING;
	    else {
		--whilestack_ptr;
		for (i = 1; i != 0; ) {
		    t = skip_to_while_wend(t);
		    if (*t == EOL)
			in_if = 0;
		    else if (*t == WHILE)
			++i;
		    else if (*t == WEND)
			--i;
		    else if (*t == EOP)
			error(ERROR_WHILE);
		    ++t;
		}
	    }
	    continue;
	case WEND:
	    if (whilestack_ptr == 0)
		error(ERROR_WHILE);
	    t = whilestack[--whilestack_ptr];
		continue;
	case LPRINT:
	    select_file(PRINTER);
	    /* drop through to PRINT */
	case PRINT:
	    ++t;
	    p = NULL;
	    valuep = NULL;
	    if (token_optional(&t, HASH)) {
		if (is_string_variable(t)) {
		    outdev = -99;
		    p = t;
		    valuep =
			dimensioned_variable(&t);
		    coutmax = 1024;
		    coutp =
			get_memory_atomic(coutmax);
		    coutn = 0;
		    ppos = &coutn;
		} else
		    select_file(expression(&t));
	    }
	    if (token_optional(&t, USING)) {
		t = print_using(t);
		/* Skip any unused expressions */
		t = skip_statement(t) - 1;
	    } else
		t = print(t);
	    if (p) {
		s = make_string(0);
		s.storage = coutp;
		s.length = coutn;
		store_string(p, valuep, s);
		coutp = NULL;
	    } else {
		if (t[-1] != SEMI)
		    crlf();
	    }
	    break;
	case WRITE:
	    ++t;
	    if (token_optional(&t, HASH))
		select_file(expression(&t));
	    t = handle_write(t);
	    break;
	case RANDOMIZE:
	    ++t;
	    srand((int)expression(&t));
	    break;
	case FOR:
	    /* FOR I = 1 TO 10 [STEP 1]...NEXT [I] */
	    for (i = 0; i < forstack_ptr; ++i)
		if (forstack[i] == t) {
		    forstack_ptr = i;
		break;
		}
	    if (forstack_ptr == FOR_MAX)
		error(ERROR_FOR);
	    forstack[forstack_ptr++] = t;
	    p = ++t;
	    token_needed(t, VARIABLE);
	    t = skip_variable(t);
	    eat_token(&t, EQUAL);
	    set_variable(p, expression(&t));
	    t = skip_statement(t) - 1;
	    /* FIXME - see WHILE for zero-trip */
	    p = forstack[forstack_ptr - 1];
	    p2 = ++p;
	    x = get_variable(&p);
	    ++p;
	    expression(&p);
	    eat_token(&p, TO);
	    x1 = expression(&p);
	    if (token_optional(&p, STEP))
		x2 = expression(&p);
	    else
		x2 = 1.0;
	    if ((x2 > 0) ? (x <= x1) : (x >= x1))
		NOTHING;
	    else {
		--forstack_ptr;
		for (i = 1; i != 0; ) {
		    t = skip_to_for_next(t);
		    if (*t == EOL)
			in_if = 0;
		    else if (*t == FOR)
			++i;
		    else if (*t == NEXT)
			--i;
		    else if (*t == EOP)
			error(ERROR_FOR);
		    ++t;
		}
		t = skip_statement(t) - 1;
	    }
	    break;
	case NEXT:
	    if (forstack_ptr == 0)
		error(ERROR_FOR);
	    ++t;
	    p = forstack[--forstack_ptr];
	    if (*t == VARIABLE) {
		while (forstack_ptr &&
		       (strcmp((char *)t+2,
		       (char *)p+3) != 0))
		    p = forstack[--forstack_ptr];
		if (strcmp((char *)t+2,
		   (char *)p+3) != 0)
		    error(ERROR_FOR);
		t = skip_variable(t);
	    }
	    forstack[forstack_ptr++] = p;
	    p2 = ++p;
	    x = get_variable(&p);
	    ++p;
	    expression(&p);
	    eat_token(&p, TO);
	    x1 = expression(&p);
	    if (token_optional(&p, STEP))
		x2 = expression(&p);
	    else
		x2 = 1.0;
	    x = x + x2;
	    set_variable(p2, x);
	    if ((x2 > 0) ? (x <= x1) : (x >= x1))
		t = p;
	    else
		--forstack_ptr;
	    break;
	case ERROR:
	    ++t;
	    error((int)expression(&t));
	    break;
	case RESTORE:
	    ++t;
	    if (!statement_end(t))
		data = find_line_strict(expression(&t));
	    else
		data = sop();
	    break;
	case DIM:
	    do
		t = dim(++t);
	    while (*t == COMMA);
	    break;
	case POKE:
	    i = 0;
poke_out:   ++t;
	    x1 = expression(&t);
	    comma(&t);
	    x2 = expression(&t);
	    if (i == 0)
		poke((size_t)x1, (unsigned char)x2);
	    else
		out((int)x1, (unsigned char)x2);
	    break;
	case OUT:
	    i = 1;
	    goto poke_out;
	case LSET:
	    i = 0;
set_lr:     ++t;
	    need_string_variable(t);
	    p = t;
	    t = skip_variable(t);
	    eat_token(&t, EQUAL);
	    s = sexpression(&t);
	    lr_set_string(p, s, i);
	    break;
	case RSET:
	    i = 1;
	    goto set_lr;
	case ERASE:
	    do
		t = ferase(++t);
	    while (*t == COMMA);
	    break;
	case COMMON:
	    do
		t = common(++t);
	    while (*t == COMMA);
	    break;
	case BEEP:
	    ++t;
	    if (use_curses)
		beep();
	    else
		cout(7);
	    break;
	case SLEEP:
	    ++t;
	    sleep((int)expression(&t));
	    break;
	case SWAP:
	    ++t;
	    token_needed(t, VARIABLE);
	    i = is_string_variable(t);
	    valuep = dimensioned_variable(&t);
	    if (valuep == NULL) {
		if (i)
		    valuep = address_string(
			(char *)p+2);
		    else
			valuep = address_scalar(
			    (char *)p+2);
	    }
	    comma(&t);
	    token_needed(t, VARIABLE);
	    if (is_string_variable(p))
		if (!is_string_variable(t))
		    error(ERROR_SYNTAX);
	    if (!is_string_variable(p))
		if (is_string_variable(t))
		    error(ERROR_SYNTAX);
	    valuep2 = dimensioned_variable(&t);
	    if (valuep2 == NULL) {
		if (i)
		    valuep2 = address_string(
					    (char *)p+2);
		else
		    valuep2 = address_scalar(
					    (char *)p+2);
	    }
	    if (i) {
		s = *(basic_string *)valuep;
		*(basic_string *)valuep =
		    *(basic_string *)valuep2;
		*(basic_string *)valuep2 = s;
	    } else {
		x = *(double *)valuep;
		*(double *)valuep = *(double *)valuep2;
		*(double *)valuep2 = x;
	    }
	    break;
	case OPEN:
	    /* OPEN "IORAB", #n, fn, rlen */
	    ++t;
	    {
	    int mode;
	    basic_string filename;
	    int filenumber, reclen;

	    s = sexpression(&t);
	    if (s.length == 0)
		error(ERROR_SYNTAX);
	    mode = s.storage[0];
	    if (('a' <= mode) && (mode <= 'z'))
		mode = mode - 'a' + 'A';
	    switch (mode) {
		case 'I': mode = MODE_INPUT; break;
		case 'O': mode = MODE_OUTPUT; break;
		case 'R': mode = MODE_RANDOM; break;
		case 'A': mode = MODE_APPEND; break;
		case 'B': mode = MODE_BINARY; break;
		default: error(ERROR_SYNTAX);
	    }
	    comma(&t);
	    token_optional(&t, HASH);
	    filenumber = expression(&t);
	    comma(&t);
	    filename = sexpression(&t);
	    reclen = 128;
	    if (token_optional(&t, COMMA))
		reclen = expression(&t);
	    open_file(mode, filenumber, cstring(filename),
		      reclen);
	    }
	    break;
	case CLOSE:
	    ++t;
	    if (statement_end(t)) {
		for (i = 0; i < MAX_FD; ++i)
		    close_file(i);
	    } else {
		--t;
		do {
		    ++t;
		    token_optional(&t, HASH);
		    x = expression(&t);
		    close_file((int)x);
		} while (*t == COMMA);
	    }
	    break;
	case INPUT:
	    ++t;
	    if ((*t == HASH) || (*t == CONSTANT)) {
		token_optional(&t, HASH);
		select_file(expression(&t));
		comma(&t);
	    }
	    t = input(t);
	    break;
	case LINE:
	    /* LINE INPUT */
	    ++t;
	    eat_token(&t, INPUT);
	    if ((*t == HASH) || (*t == CONSTANT)) {
		token_optional(&t, HASH);
		select_file(expression(&t));
		comma(&t);
	    }
	    t = line_input(t);
	    break;
	case FIELD:
	    {
	    int fn, n;
	    char *buf;
	    basic_string s;

	    ++t;
	    if (token_optional(&t, ON)) {
		buf = (char *)(size_t)expression(&t);
	    } else {
		token_optional(&t, HASH);
		fn = expression(&t);
		buf = files[fn].buffer;
	    }
	    comma(&t);
	    FOREVER {
		if (statement_end(t))
		    break;
		n = expression(&t);
		eat_token(&t, AS);
		need_string_variable(t);
		t += 2;
		s.length = n;
		s.storage = buf;
		create_string((char *)t, s);
		buf += n;
		t += t[-1];	/* XXX */
		token_optional(&t, COMMA);
	    }
	    }
	    break;
	case GET:
	    {
	    int fn;
	    int rec;

	    ++t;
	    token_optional(&t, HASH);
	    fn = expression(&t);
	    comma(&t);
	    rec = expression(&t);
	    if (files[fn].mode == MODE_RANDOM) {
		fseek(pfile(fn),
		  files[fn].reclen * rec,
		  SEEK_SET);
		fread(files[fn].buffer,
		  files[fn].reclen,
		  1,
		  pfile(fn));
	    } else
		error(ERROR_IO);
	    }
	    break;
	case PUT:
	    {
	    int fn;
	    int rec;

	    ++t;
	    token_optional(&t, HASH);
	    fn = expression(&t);
	    comma(&t);
	    rec = expression(&t);
	    if (files[fn].mode == MODE_RANDOM) {
		fseek(pfile(fn),
		      files[fn].reclen * rec,
		      SEEK_SET);
		fwrite(files[fn].buffer,
		       files[fn].reclen,
		       1,
		       pfile(fn));
	    } else
		error(ERROR_IO);
	    }
	    break;
	case DEF:
	    ++t;
	    if (*t == INT) {
		do
		    t = handle_deftype('%', ++t);
		while (*t == COMMA);
	    } else if (*t == SNG) {
		do
		    t = handle_deftype('!', ++t);
		while (*t == COMMA);
	    } else if (*t == FN) {	/* FN */
		/* Function definition, record the
		 * start token location
		 */
		++t;
		token_needed(t, VARIABLE);
		fns[t[2]] = t;
		t = skip_statement(t) - 1;
	    } else if (*t == STR) {
		do
		    t = handle_deftype('$', ++t);
		while (*t == COMMA);
	    } else if (*t == DBL) {
		do
		    t = handle_deftype('!', ++t);
		while (*t == COMMA);
	    } else if (*t == SEG) {
		++t;
		token_needed(t, EQUAL);
		seg = expression(&t);
	    } else
		error(ERROR_SYNTAX);
	    break;
	case CLEAR:
	    t = skip_statement(t) - 1;
	    fclear();
	    break;
	case FILES:
	    /* FILES -- pass string to ls command */
	    ++t;
	    if (!statement_end(t)) {
		s = sexpression(&t);
		sp = cstring(s);
	    } else
		sp = "";
	    external_execute("ls", sp);
	    break;
	case KILL:
	    ++t;
	    s = sexpression(&t);
	    sp = cstring(s);
	    if (unlink(sp) == -1)
		error(ERROR_IO);
	    break;
	case VARS:
	    t = skip_statement(t) - 1;
	    vars();
	    break;
	case CURSESF:
	    {
	    int f;

	    ++t;
	    if (*t == ON) {
		++t;
		f = 1;
	    } else if (*t == OFF) {
		++t;
		f = 0;
	    } else
		f = expression(&t);
	    if (!use_curses && f)
		enter_curses();
	    else if (use_curses && !f) {
		exit_cursesx();
		ansi_cls();
	    }
	    }
	    break;
	case RESUME:
	    ++t;
	    if (!inerror)
		error(ERROR_SYNTAX);
	    running = YES;
	    inerror = NO;
	    if (token_optional(&t, NEXT))
		t = skip_statement(current_loc);
	    else if (statement_end(t))
		t = current_loc;
	    else {
		x = onerror;
		onerror = 0;
		t = find_line_strict(expression(&t));
		onerror = x;
	    }
	    continue;
	    break;

	case ON:
	    {
	    double idx;
	    int mode;

	    ++t;
	    if (*t == TIMER) { /* ON TIMER(x) GOSUB */
		++t;
		idx = expression(&t);
		secondsleft = ontimevalue = (int)idx;
		eat_token(&t, GOSUB);
		ontimer = t;
		t = skip_statement(t) - 1;
		time(&current_time);
		if (ontimevalue <= 0) {
		    ontimer = NULL;
		    ontimerflag = NO;
		} else
		    ontimerflag = YES;
		break;	/* case */
	    } else if (*t == 32) { /* ON ERROR */
		++t;
		if (token_optional(&t, RESUME)) {
		    eat_token(&t, NEXT);
		    /* ON ERROR RESUME NEXT */
		    onerror = -1;
		} else {
		    eat_token(&t, GOTO);
		    /* ON ERROR GOTO */
		    onerror = expression(&t);
		    if (onerror != 0)
			find_line_strict(
					onerror);
		}
		break; /* case */
	    }

	    idx = intx(expression(&t));
	    if (*t == GOSUB) { /* ON e GOSUB */
		++t;
		mode = 1;
	    } else if (*t == GOTO) { /* ON e GOTO */
		++t;
		mode = 2;
	    } else
		error(ERROR_SYNTAX);
	    for (; idx > 1; --idx) {
		expression(&t);
		token_optional(&t, COMMA);
		if (statement_end(t))
		    break;
	    }
	    if (idx == 1) {
		if (mode == 1) {
		    if (gosubstack_ptr >= GOSUB_MAX)
			error(ERROR_GOSUB);
		    gosubstack[gosubstack_ptr++] =
			skip_statement(t);
		}
		goto bgoto;
	    }
	    t = skip_statement(t) - 1;
	    }
	    break;
	case TIMER:	/* TIMER ON, OFF, STOP */
	    ++t;
	    if (*t == ON) {
		if (ontimer)
		    ontimerflag = YES;
	    } else if (*t == OFF) {
		ontimerflag = NO;
	    } else if (*t == STOP) {
		ontimerflag = NO;
		ontimer = NULL;
	    } else
		error(ERROR_SYNTAX);
	    ++t;
	    break;
	case LOCATE:
	    /* LOCATE row,col,cursor,start,stop */
	    if (use_curses) {
		int row = 0, col = 0, crs = 0;
		int have_row = NO;
		int have_col = NO;
		int have_crs = NO;
		int y, x;

		++t;
		if ((*t != COMMA) && !statement_end(t)) {
		    row = expression(&t) - 1;
		    have_row = YES;
		}
		token_optional(&t, COMMA);
		if ((*t != COMMA) && !statement_end(t)) {
		    col = expression(&t) - 1;
		    have_col = YES;
		}
		token_optional(&t, COMMA);
		if ((*t != COMMA) && !statement_end(t)) {
		    crs = expression(&t);
		    have_crs = YES;
		}
		token_optional(&t, COMMA);
		getyx(sc, y, x);
		if (!have_row)
		    row = y;
		if (!have_col)
		    col = x;
		move(row, col);
		if (have_crs)
		    curs_set(crs);
	    }
	    t = skip_statement(t) - 1;
	    break;
	case CLS:
	    ++t;
	    if (use_curses) {
		erase();
		move(0, 0);
	    } else
		ansi_cls();
	    break;
	case COLOR:
	    ++t;
	    if (use_curses) {
		int n, f, b;

		f = expression(&t);
		comma(&t);
		b = expression(&t);
		n = ((b & 7) << 3) + (f & 7);
		/* color_set(n, NULL) is not supported
		 * under Solaris
		 */
		attrset(COLOR_PAIR(n));
		bkgdset(' ' | COLOR_PAIR(n));
	    } else
		t = skip_statement(t) - 1;
	    break;
	case NULLF:
	    ++t;
	    nulls = expression(&t);
	    break;
	case CHAIN:
	    /* CHAIN [MERGE] filename
	     *	[,[line exp][,ALL][,DELETE range]]
	     */
	    {
	    int mergeflag = NO;
	    int allflag = NO;
	    basic_string filename;

	    ++t;
	    x = -1;
	    only_common_variables();
	    if (*t == MERGE) {
		if (noedit)
		    error(ERROR_NOEDIT);
		++t;
		mergeflag = YES;
	    }
	    filename = sexpression(&t);
	    while (token_optional(&t, COMMA)) {
		if (*t == ALL) {
		    ++t;
		    allflag = YES;
		} else if (*t == DELETE) {
		    if (noedit)
			error(ERROR_NOEDIT);
		    ++t;
		    t = delete_range(t);
		} else if (*t == COMMA)
		    ;
		else
		    x = expression(&t);
	    }

	    if (!allflag)
		only_common_variables();

	    if (mergeflag)
		merge(cstring(filename));
	    else
		load(cstring(filename));

	    if (x <= 0)
		t = sop();
	    else
		t = find_line_strict(x);
	    clear_execution_state();
	    in_if = 0;
	    running = YES;
	    continue;
	    }
	case WAIT:
	    /* WAIT port,and[,xor]*/
	    ++t;
	    x = expression(&t);
	    comma(&t);
	    x1 = expression(&t);
	    if (token_optional(&t, COMMA))
		x2 = expression(&t);
	    else
		x2 = 0;
	    waitport(x, x1, x2);
	    break;
	case ENVIRON:
	    ++t;
	    s = sexpression(&t);
	    sp = get_memory_atomic(strlen(cstring(s))+1);
	    strcpy(sp, cstring(s));
	    putenv(sp);
	    free_memory(sp);
	    break;
	case CHDIR:
	    ++t;
	    s = sexpression(&t);
	    sp = cstring(s);
	    if (chdir(sp) == -1)
		error(ERROR_IO);
	    break;
	case MKDIR:
	    ++t;
	    s = sexpression(&t);
	    sp = cstring(s);
	    if (mkdir(sp, 0777) == -1)
		error(ERROR_IO);
	    break;
	case RMDIR:
	    ++t;
	    s = sexpression(&t);
	    sp = cstring(s);
	    if (rmdir(sp) == -1)
		error(ERROR_IO);
	    break;
	case NAME:
	    ++t;
	    s = sexpression(&t);
	    eat_token(&t, AS);
	    s2 = sexpression(&t);
	    if (rename(cstring(s), cstring(s2)) == -1)
		error(ERROR_IO);
	    break;
	case WIDTH:
	    ++t;
	    pi = &width;
	    if (token_optional(&t, LPRINT))
		pi = &lwidth;
	    else if (token_optional(&t, HASH)) {
		pi = &(files[(int)expression(&t)].width);
		comma(&t);
	    }
	    *pi = expression(&t);
	    /* MBASIC uses 255 as a "magic" number. We use 0, but translate
	     * here.
	     */
	    if (*pi == 255)
		*pi = 0;
	    break;
	case RENUM:
	    x = 0;
	    goto ren_aut;
	case AUTO:
	    x = 1;
ren_aut:    ++t;
	    x1 = 10;
	    x2 = 10;
	    if (!statement_end(t)) {
		if (*t != COMMA)
		    x1 = expression(&t);
		if (token_optional(&t, COMMA)) {
		    if (!statement_end(t))
			x2 = expression(&t);
		}
	    }
	    if (x == 1)
		do_auto(x1, x2);
	    else
		do_renum(x1, x2);
	    break;
	case MID:
assign_mid: ++t;
	    /* MID$(V$,P,L)=SEXP$ */
	    need_string_variable(t);
	    s2 = get_string_variable(&t);
	    eat_token(&t, COMMA);
	    x1 = expression(&t);
	    if (token_optional(&t, COMMA))
		x2 = expression(&t);
	    else
		x2 = 999999;
	    closep(&t);
	    eat_token(&t, EQUAL);
	    s = sexpression(&t);
	    for (i = (int)x1; i < (int)(x1 + x2); ++i) {
		if (i > s2.length)
		    break;
		if ((i - (int)x1) >= s.length)
		    break;
		s2.storage[i-1] = s.storage[i-(int)x1];
	    }
	    break;
	case INTERPRET:
	    /* We don't use tkn_buf, because that will be
	     * in use if someone trys INTERPRET from
	     * keyboard directly
	     */
	    {
	    unsigned char *tb = get_memory_atomic(MAX_TKN);

	    ++t;
	    s = sexpression(&t);
	    tokenize_line(cstring(s), tb);
	    execute(tb);
	    free_memory(tb);
	    }
	    break;
	default:
	    error(ERROR_SYNTAX);
	}
	select_file(CONSOLE);
	flush_out();

	/* Since we have just executed a statement, the token
	 * must be end of line, end of program, or ':'
	 */
	if (*t == EOP)
	    return;
	if (*t == EOL)
	    in_if = 0;
	if (*t == ELSE) {
	    if (in_if) {
		t = skip(t);
	    continue;
	    } else
		error(ERROR_SYNTAX);
	}
	if ((*t != EOL) && (*t != COLON))
	    error(ERROR_SYNTAX);
	++t;
    }
}

/* Entry for FBASIC interpreter.
 *
 * Allocates a program space, and then enters a read edit/interpret
 * loop.
 */

int main(int ac, char **av) {
    int prompt;
    int n;

    g_ac = ac;
    g_av = av;

    noedit = NO;
    nulls = 0;
    program = get_memory_atomic(MAX_PROGRAM);
    program[0] = START;
    program[1] = EOP;
    variables = NULL;
    use_curses = NO;
    jbreakflag = NO;
    select_file(CONSOLE);
    signal(SIGINT, signal_break);

    clear_execution_state();
    fclear();

    n = strlen(av[0]);
    frun = NO;
    current_line = 0;
    if (n >= 3) {
	/* if invoked as "frun", or any name ending in "run",
	 * enter frun mode
	 */
	if (strcmp(av[0]+n-3, "run") == 0)
		frun = YES;
    }

    if (!frun) {
	sout("FBASIC " VERSION); crlf();
	sout("Written 2000, 2001, 2022 Fridtjof Weigel"); crlf();
	sout("MIT Licensed"); crlf();
	crlf();
    }

    /* main command loop, get a line, tokenize it. If it has a
     * line number, edit the program, else execute it.
     */

    if (setjmp(jerror) != 0) {
	if ((!inerror) && (onerror != 0) && (running)) {
	    /* XXX use the same mechanism to spring TIMER? */
	    if (onerror < 0) { /* -1 == RESUME NEXT */
		inerror = NO;
		execute(skip_statement(current_loc) - 1);
	    } else {
		inerror = YES;
		tkn_buf[0] = GOTO;
		tkn_buf[1] = CONSTANT;
		W(tkn_buf+2, onerror);
		tkn_buf[2+sizeof(double)] = EOL;
		tkn_buf[3+sizeof(double)] = EOP;
		execute(tkn_buf);
	    }
	} else {
	    select_file(CONSOLE);
	    crlf();
	    sprintf(src, "Error: %s (%.10G)", error_message(), err);
	    sout(src);
	    if (running) {
		running = NO;
		sprintf(src, " in line %.10G", erl);
		sout(src);
	    }
	    crlf();
	}
	if (frun) {
	    exit_cursesx();
	    exit(1);
	} else
	    goto cont;
    }

    if (frun) {
	if (ac == 1) {
	    sout("Missing filename");
	    crlf();
	} else {
	    running = NO;
	    load(av[1]);
	    tkn_buf[0] = RUN;
	    tkn_buf[1] = EOL;
	    tkn_buf[2] = EOP;
	    execute(tkn_buf);
	}
	exit_cursesx();
	return 0;
    }

    if (ac > 1)
	load(av[1]);

cont:
    prompt = YES;
    FOREVER {
	running = NO;
	if (prompt) {
	    sout("Ok");
	    crlf();
	}
	get_line(src, sizeof(src));
	tokenize_line(src, tkn_buf);
	if (tkn_buf[0] == CONSTANT) {
	    if (noedit)
		error(ERROR_NOEDIT);
	    edit_program(tkn_buf);
	    prompt = NO;
	} else {
	    if ((tkn_buf[0] == EOL) || ((tkn_buf[0] == EDIT) &&
					(tkn_buf[1] != EOL)))
		prompt = NO;
	    else
		prompt = YES;
		execute(tkn_buf);
	}
    }
    exit_cursesx();
    return 0;
}
