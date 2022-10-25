/* A Bison parser, made by GNU Bison 3.7.4.  */

<<<<<<< HEAD
/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with FF_ or ff_.  They are
   private implementation details that can be changed or removed.  */

=======
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
/* All symbols defined below should begin with ff or FF, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

<<<<<<< HEAD
/* Identify Bison output, and Bison version.  */
#define FFBISON 30704

/* Bison version string.  */
#define FFBISON_VERSION "3.7.4"
=======
/* Identify Bison output.  */
#define FFBISON 1

/* Bison version.  */
#define FFBISON_VERSION "2.4.1"
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0

/* Skeleton name.  */
#define FFSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define FFPURE 0

/* Push parsers.  */
#define FFPUSH 0

/* Pull parsers.  */
#define FFPULL 1

<<<<<<< HEAD



/* First part of user prologue.  */
=======
/* Using locations.  */
#define FFLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
#line 1 "eval.y"

/************************************************************************/
/*                                                                      */
/*                       CFITSIO Lexical Parser                         */
/*                                                                      */
/* This file is one of 3 files containing code which parses an          */
/* arithmetic expression and evaluates it in the context of an input    */
/* FITS file table extension.  The CFITSIO lexical parser is divided    */
/* into the following 3 parts/files: the CFITSIO "front-end",           */
/* eval_f.c, contains the interface between the user/CFITSIO and the    */
/* real core of the parser; the FLEX interpreter, eval_l.c, takes the   */
/* input string and parses it into tokens and identifies the FITS       */
/* information required to evaluate the expression (ie, keywords and    */
/* columns); and, the BISON grammar and evaluation routines, eval_y.c,  */
/* receives the FLEX output and determines and performs the actual      */
/* operations.  The files eval_l.c and eval_y.c are produced from       */
/* running flex and bison on the files eval.l and eval.y, respectively. */
/* (flex and bison are available from any GNU archive: see www.gnu.org) */
/*                                                                      */
/* The grammar rules, rather than evaluating the expression in situ,    */
/* builds a tree, or Nodal, structure mapping out the order of          */
/* operations and expression dependencies.  This "compilation" process  */
/* allows for much faster processing of multiple rows.  This technique  */
/* was developed by Uwe Lammers of the XMM Science Analysis System,     */
/* although the CFITSIO implementation is entirely code original.       */
/*                                                                      */
/*                                                                      */
/* Modification History:                                                */
/*                                                                      */
/*   Kent Blackburn      c1992  Original parser code developed for the  */
/*                              FTOOLS software package, in particular, */
/*                              the fselect task.                       */
/*   Kent Blackburn      c1995  BIT column support added                */
/*   Peter D Wilson   Feb 1998  Vector column support added             */
/*   Peter D Wilson   May 1998  Ported to CFITSIO library.  User        */
/*                              interface routines written, in essence  */
/*                              making fselect, fcalc, and maketime     */
/*                              capabilities available to all tools     */
/*                              via single function calls.              */
/*   Peter D Wilson   Jun 1998  Major rewrite of parser core, so as to  */
/*                              create a run-time evaluation tree,      */
/*                              inspired by the work of Uwe Lammers,    */
/*                              resulting in a speed increase of        */
/*                              10-100 times.                           */
/*   Peter D Wilson   Jul 1998  gtifilter(a,b,c,d) function added       */
/*   Peter D Wilson   Aug 1998  regfilter(a,b,c,d) function added       */
/*   Peter D Wilson   Jul 1999  Make parser fitsfile-independent,       */
/*                              allowing a purely vector-based usage    */
/*  Craig B Markwardt Jun 2004  Add MEDIAN() function                   */
/*  Craig B Markwardt Jun 2004  Add SUM(), and MIN/MAX() for bit arrays */
/*  Craig B Markwardt Jun 2004  Allow subscripting of nX bit arrays     */
/*  Craig B Markwardt Jun 2004  Implement statistical functions         */
/*                              NVALID(), AVERAGE(), and STDDEV()       */
/*                              for integer and floating point vectors  */
/*  Craig B Markwardt Jun 2004  Use NULL values for range errors instead*/
/*                              of throwing a parse error               */
/*  Craig B Markwardt Oct 2004  Add ACCUM() and SEQDIFF() functions     */
/*  Craig B Markwardt Feb 2005  Add ANGSEP() function                   */
/*  Craig B Markwardt Aug 2005  CIRCLE, BOX, ELLIPSE, NEAR and REGFILTER*/
/*                              functions now accept vector arguments   */
/*  Craig B Markwardt Sum 2006  Add RANDOMN() and RANDOMP() functions   */
/*  Craig B Markwardt Mar 2007  Allow arguments to RANDOM and RANDOMN to*/
/*                              determine the output dimensions         */
/*  Craig B Markwardt Aug 2009  Add substring STRMID() and string search*/
/*                              STRSTR() functions; more overflow checks*/
<<<<<<< HEAD
/*  Craig B Markwardt Dec 2019  Add bit/hex/oct literal strings and     */
/*                              bitwise operatiosn between integers     */
/*  Craig B Markwardt Mar 2021  Add SETNULL() function                  */
=======
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
/*                                                                      */
/************************************************************************/

#define  APPROX 1.0e-7
#include "eval_defs.h"
#include "region.h"
#include <time.h>

#include <stdlib.h>

#ifndef alloca
#define alloca malloc
#endif

/* Random number generators for various distributions */
#include "simplerng.h"

   /*  Shrink the initial stack depth to keep local data <32K (mac limit)  */
   /*  yacc will allocate more space if needed, though.                    */
#define  FFINITDEPTH   100

/***************************************************************/
/*  Replace Bison's BACKUP macro with one that fixes a bug --  */
/*  must update state after popping the stack -- and allows    */
/*  popping multiple terms at one time.                        */
/***************************************************************/

#define FFNEWBACKUP(token, value)                               \
   do								\
     if (ffchar == FFEMPTY )   					\
       { ffchar = (token);                                      \
         memcpy( &fflval, &(value), sizeof(value) );            \
         ffchar1 = FFTRANSLATE (ffchar);			\
         while (fflen--) FFPOPSTACK;				\
         ffstate = *ffssp;					\
         goto ffbackup;						\
       }							\
     else							\
       { fferror ("syntax error: cannot back up"); FFERROR; }	\
   while (0)

/***************************************************************/
/*  Useful macros for accessing/testing Nodes                  */
/***************************************************************/

#define TEST(a)        if( (a)<0 ) FFERROR
#define SIZE(a)        gParse.Nodes[ a ].value.nelem
#define TYPE(a)        gParse.Nodes[ a ].type
#define OPER(a)        gParse.Nodes[ a ].operation
#define PROMOTE(a,b)   if( TYPE(a) > TYPE(b) )                  \
                          b = New_Unary( TYPE(a), 0, b );       \
                       else if( TYPE(a) < TYPE(b) )             \
	                  a = New_Unary( TYPE(b), 0, a );

/*****  Internal functions  *****/

#ifdef __cplusplus
extern "C" {
#endif

static int  Alloc_Node    ( void );
static void Free_Last_Node( void );
static void Evaluate_Node ( int thisNode );

static int  New_Const ( int returnType, void *value, long len );
static int  New_Column( int ColNum );
static int  New_Offset( int ColNum, int offset );
static int  New_Unary ( int returnType, int Op, int Node1 );
static int  New_BinOp ( int returnType, int Node1, int Op, int Node2 );
static int  New_Func  ( int returnType, funcOp Op, int nNodes,
			int Node1, int Node2, int Node3, int Node4, 
			int Node5, int Node6, int Node7 );
static int  New_FuncSize( int returnType, funcOp Op, int nNodes,
			int Node1, int Node2, int Node3, int Node4, 
			  int Node5, int Node6, int Node7, int Size);
static int  New_Deref ( int Var,  int nDim,
			int Dim1, int Dim2, int Dim3, int Dim4, int Dim5 );
static int  New_GTI   ( funcOp Op, char *fname, int Node1, int Node2, char *start, char *stop );
static int  New_REG   ( char *fname, int NodeX, int NodeY, char *colNames );
static int  New_Vector( int subNode );
static int  Close_Vec ( int vecNode );
static int  Locate_Col( Node *this );
static int  Test_Dims ( int Node1, int Node2 );
static void Copy_Dims ( int Node1, int Node2 );

static void Allocate_Ptrs( Node *this );
static void Do_Unary     ( Node *this );
static void Do_Offset    ( Node *this );
static void Do_BinOp_bit ( Node *this );
static void Do_BinOp_str ( Node *this );
static void Do_BinOp_log ( Node *this );
static void Do_BinOp_lng ( Node *this );
static void Do_BinOp_dbl ( Node *this );
static void Do_Func      ( Node *this );
static void Do_Deref     ( Node *this );
static void Do_GTI       ( Node *this );
static void Do_GTI_Over  ( Node *this );
static void Do_REG       ( Node *this );
static void Do_Vector    ( Node *this );

static long Search_GTI   ( double evtTime, long nGTI, double *start,
			   double *stop, int ordered, long *nextGTI );
static double GTI_Over(double evtStart, double evtStop,
		       long nGTI, double *start, double *stop,
		       long *gtiout);

static char  saobox (double xcen, double ycen, double xwid, double ywid,
		     double rot,  double xcol, double ycol);
static char  ellipse(double xcen, double ycen, double xrad, double yrad,
		     double rot, double xcol, double ycol);
static char  circle (double xcen, double ycen, double rad,
		     double xcol, double ycol);
static char  bnear  (double x, double y, double tolerance);
static char  bitcmp (char *bitstrm1, char *bitstrm2);
static char  bitlgte(char *bits1, int oper, char *bits2);

static void  bitand(char *result, char *bitstrm1, char *bitstrm2);
static void  bitor (char *result, char *bitstrm1, char *bitstrm2);
static void  bitnot(char *result, char *bits);
static int cstrmid(char *dest_str, int dest_len,
		   char *src_str,  int src_len, int pos);

static void  fferror(char *msg);

#ifdef __cplusplus
    }
#endif


<<<<<<< HEAD
#line 269 "y.tab.c"

# ifndef FF_CAST
#  ifdef __cplusplus
#   define FF_CAST(Type, Val) static_cast<Type> (Val)
#   define FF_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define FF_CAST(Type, Val) ((Type) (Val))
#   define FF_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef FF_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define FF_NULLPTR nullptr
#   else
#    define FF_NULLPTR 0
#   endif
#  else
#   define FF_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef FF_FF_Y_TAB_H_INCLUDED
# define FF_FF_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef FFDEBUG
# define FFDEBUG 0
#endif
#if FFDEBUG
extern int ffdebug;
#endif

/* Token kinds.  */
#ifndef FFTOKENTYPE
# define FFTOKENTYPE
  enum fftokentype
  {
    FFEMPTY = -2,
    FFEOF = 0,                     /* "end of file"  */
    FFerror = 256,                 /* error  */
    FFUNDEF = 257,                 /* "invalid token"  */
    BOOLEAN = 258,                 /* BOOLEAN  */
    LONG = 259,                    /* LONG  */
    DOUBLE = 260,                  /* DOUBLE  */
    STRING = 261,                  /* STRING  */
    BITSTR = 262,                  /* BITSTR  */
    FUNCTION = 263,                /* FUNCTION  */
    BFUNCTION = 264,               /* BFUNCTION  */
    IFUNCTION = 265,               /* IFUNCTION  */
    GTIFILTER = 266,               /* GTIFILTER  */
    GTIOVERLAP = 267,              /* GTIOVERLAP  */
    REGFILTER = 268,               /* REGFILTER  */
    COLUMN = 269,                  /* COLUMN  */
    BCOLUMN = 270,                 /* BCOLUMN  */
    SCOLUMN = 271,                 /* SCOLUMN  */
    BITCOL = 272,                  /* BITCOL  */
    ROWREF = 273,                  /* ROWREF  */
    NULLREF = 274,                 /* NULLREF  */
    SNULLREF = 275,                /* SNULLREF  */
    OR = 276,                      /* OR  */
    AND = 277,                     /* AND  */
    EQ = 278,                      /* EQ  */
    NE = 279,                      /* NE  */
    GT = 280,                      /* GT  */
    LT = 281,                      /* LT  */
    LTE = 282,                     /* LTE  */
    GTE = 283,                     /* GTE  */
    XOR = 284,                     /* XOR  */
    POWER = 285,                   /* POWER  */
    NOT = 286,                     /* NOT  */
    INTCAST = 287,                 /* INTCAST  */
    FLTCAST = 288,                 /* FLTCAST  */
    UMINUS = 289,                  /* UMINUS  */
    ACCUM = 290,                   /* ACCUM  */
    DIFF = 291                     /* DIFF  */
  };
  typedef enum fftokentype fftoken_kind_t;
#endif
/* Token kinds.  */
#define FFEMPTY -2
#define FFEOF 0
#define FFerror 256
#define FFUNDEF 257
=======

/* Line 189 of yacc.c  */
#line 265 "y.tab.c"

/* Enabling traces.  */
#ifndef FFDEBUG
# define FFDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef FFERROR_VERBOSE
# undef FFERROR_VERBOSE
# define FFERROR_VERBOSE 1
#else
# define FFERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef FFTOKEN_TABLE
# define FFTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef FFTOKENTYPE
# define FFTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum fftokentype {
     BOOLEAN = 258,
     LONG = 259,
     DOUBLE = 260,
     STRING = 261,
     BITSTR = 262,
     FUNCTION = 263,
     BFUNCTION = 264,
     IFUNCTION = 265,
     GTIFILTER = 266,
     REGFILTER = 267,
     COLUMN = 268,
     BCOLUMN = 269,
     SCOLUMN = 270,
     BITCOL = 271,
     ROWREF = 272,
     NULLREF = 273,
     SNULLREF = 274,
     OR = 275,
     AND = 276,
     NE = 277,
     EQ = 278,
     GTE = 279,
     LTE = 280,
     LT = 281,
     GT = 282,
     POWER = 283,
     NOT = 284,
     FLTCAST = 285,
     INTCAST = 286,
     UMINUS = 287,
     DIFF = 288,
     ACCUM = 289
   };
#endif
/* Tokens.  */
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
#define BOOLEAN 258
#define LONG 259
#define DOUBLE 260
#define STRING 261
#define BITSTR 262
#define FUNCTION 263
#define BFUNCTION 264
#define IFUNCTION 265
#define GTIFILTER 266
<<<<<<< HEAD
#define GTIOVERLAP 267
#define REGFILTER 268
#define COLUMN 269
#define BCOLUMN 270
#define SCOLUMN 271
#define BITCOL 272
#define ROWREF 273
#define NULLREF 274
#define SNULLREF 275
#define OR 276
#define AND 277
#define EQ 278
#define NE 279
#define GT 280
#define LT 281
#define LTE 282
#define GTE 283
#define XOR 284
#define POWER 285
#define NOT 286
#define INTCAST 287
#define FLTCAST 288
#define UMINUS 289
#define ACCUM 290
#define DIFF 291

/* Value type.  */
#if ! defined FFSTYPE && ! defined FFSTYPE_IS_DECLARED
union FFSTYPE
{
#line 199 "eval.y"
=======
#define REGFILTER 267
#define COLUMN 268
#define BCOLUMN 269
#define SCOLUMN 270
#define BITCOL 271
#define ROWREF 272
#define NULLREF 273
#define SNULLREF 274
#define OR 275
#define AND 276
#define NE 277
#define EQ 278
#define GTE 279
#define LTE 280
#define LT 281
#define GT 282
#define POWER 283
#define NOT 284
#define FLTCAST 285
#define INTCAST 286
#define UMINUS 287
#define DIFF 288
#define ACCUM 289




#if ! defined FFSTYPE && ! defined FFSTYPE_IS_DECLARED
typedef union FFSTYPE
{

/* Line 214 of yacc.c  */
#line 192 "eval.y"
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0

    int    Node;        /* Index of Node */
    double dbl;         /* real value    */
    long   lng;         /* integer value */
    char   log;         /* logical value */
    char   str[MAX_STRLEN];    /* string value  */

<<<<<<< HEAD
#line 402 "y.tab.c"

};
typedef union FFSTYPE FFSTYPE;
# define FFSTYPE_IS_TRIVIAL 1
=======


/* Line 214 of yacc.c  */
#line 379 "y.tab.c"
} FFSTYPE;
# define FFSTYPE_IS_TRIVIAL 1
# define ffstype FFSTYPE /* obsolescent; will be withdrawn */
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
# define FFSTYPE_IS_DECLARED 1
#endif


<<<<<<< HEAD
extern FFSTYPE fflval;

int ffparse (void);

#endif /* !FF_FF_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum ffsymbol_kind_t
{
  FFSYMBOL_FFEMPTY = -2,
  FFSYMBOL_FFEOF = 0,                      /* "end of file"  */
  FFSYMBOL_FFerror = 1,                    /* error  */
  FFSYMBOL_FFUNDEF = 2,                    /* "invalid token"  */
  FFSYMBOL_BOOLEAN = 3,                    /* BOOLEAN  */
  FFSYMBOL_LONG = 4,                       /* LONG  */
  FFSYMBOL_DOUBLE = 5,                     /* DOUBLE  */
  FFSYMBOL_STRING = 6,                     /* STRING  */
  FFSYMBOL_BITSTR = 7,                     /* BITSTR  */
  FFSYMBOL_FUNCTION = 8,                   /* FUNCTION  */
  FFSYMBOL_BFUNCTION = 9,                  /* BFUNCTION  */
  FFSYMBOL_IFUNCTION = 10,                 /* IFUNCTION  */
  FFSYMBOL_GTIFILTER = 11,                 /* GTIFILTER  */
  FFSYMBOL_GTIOVERLAP = 12,                /* GTIOVERLAP  */
  FFSYMBOL_REGFILTER = 13,                 /* REGFILTER  */
  FFSYMBOL_COLUMN = 14,                    /* COLUMN  */
  FFSYMBOL_BCOLUMN = 15,                   /* BCOLUMN  */
  FFSYMBOL_SCOLUMN = 16,                   /* SCOLUMN  */
  FFSYMBOL_BITCOL = 17,                    /* BITCOL  */
  FFSYMBOL_ROWREF = 18,                    /* ROWREF  */
  FFSYMBOL_NULLREF = 19,                   /* NULLREF  */
  FFSYMBOL_SNULLREF = 20,                  /* SNULLREF  */
  FFSYMBOL_21_ = 21,                       /* ','  */
  FFSYMBOL_22_ = 22,                       /* '='  */
  FFSYMBOL_23_ = 23,                       /* ':'  */
  FFSYMBOL_24_ = 24,                       /* '{'  */
  FFSYMBOL_25_ = 25,                       /* '}'  */
  FFSYMBOL_26_ = 26,                       /* '?'  */
  FFSYMBOL_OR = 27,                        /* OR  */
  FFSYMBOL_AND = 28,                       /* AND  */
  FFSYMBOL_EQ = 29,                        /* EQ  */
  FFSYMBOL_NE = 30,                        /* NE  */
  FFSYMBOL_31_ = 31,                       /* '~'  */
  FFSYMBOL_GT = 32,                        /* GT  */
  FFSYMBOL_LT = 33,                        /* LT  */
  FFSYMBOL_LTE = 34,                       /* LTE  */
  FFSYMBOL_GTE = 35,                       /* GTE  */
  FFSYMBOL_36_ = 36,                       /* '+'  */
  FFSYMBOL_37_ = 37,                       /* '-'  */
  FFSYMBOL_38_ = 38,                       /* '%'  */
  FFSYMBOL_39_ = 39,                       /* '*'  */
  FFSYMBOL_40_ = 40,                       /* '/'  */
  FFSYMBOL_41_ = 41,                       /* '|'  */
  FFSYMBOL_42_ = 42,                       /* '&'  */
  FFSYMBOL_XOR = 43,                       /* XOR  */
  FFSYMBOL_POWER = 44,                     /* POWER  */
  FFSYMBOL_NOT = 45,                       /* NOT  */
  FFSYMBOL_INTCAST = 46,                   /* INTCAST  */
  FFSYMBOL_FLTCAST = 47,                   /* FLTCAST  */
  FFSYMBOL_UMINUS = 48,                    /* UMINUS  */
  FFSYMBOL_49_ = 49,                       /* '['  */
  FFSYMBOL_ACCUM = 50,                     /* ACCUM  */
  FFSYMBOL_DIFF = 51,                      /* DIFF  */
  FFSYMBOL_52_n_ = 52,                     /* '\n'  */
  FFSYMBOL_53_ = 53,                       /* ']'  */
  FFSYMBOL_54_ = 54,                       /* '('  */
  FFSYMBOL_55_ = 55,                       /* ')'  */
  FFSYMBOL_FFACCEPT = 56,                  /* $accept  */
  FFSYMBOL_lines = 57,                     /* lines  */
  FFSYMBOL_line = 58,                      /* line  */
  FFSYMBOL_bvector = 59,                   /* bvector  */
  FFSYMBOL_vector = 60,                    /* vector  */
  FFSYMBOL_expr = 61,                      /* expr  */
  FFSYMBOL_bexpr = 62,                     /* bexpr  */
  FFSYMBOL_bits = 63,                      /* bits  */
  FFSYMBOL_sexpr = 64                      /* sexpr  */
=======
/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 391 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef FFTYPE_UINT8
typedef FFTYPE_UINT8 fftype_uint8;
#else
typedef unsigned char fftype_uint8;
#endif

#ifdef FFTYPE_INT8
typedef FFTYPE_INT8 fftype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char fftype_int8;
#else
typedef short int fftype_int8;
#endif

#ifdef FFTYPE_UINT16
typedef FFTYPE_UINT16 fftype_uint16;
#else
typedef unsigned short int fftype_uint16;
#endif

#ifdef FFTYPE_INT16
typedef FFTYPE_INT16 fftype_int16;
#else
typedef short int fftype_int16;
#endif

#ifndef FFSIZE_T
# ifdef __SIZE_TYPE__
#  define FFSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define FFSIZE_T size_t
# elif ! defined FFSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define FFSIZE_T size_t
# else
#  define FFSIZE_T unsigned int
# endif
#endif

#define FFSIZE_MAXIMUM ((FFSIZE_T) -1)

#ifndef FF_
# if FFENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define FF_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef FF_
#  define FF_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define FFUSE(e) ((void) (e))
#else
# define FFUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define FFID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
FFID (int ffi)
#else
static int
FFID (ffi)
    int ffi;
#endif
{
  return ffi;
}
#endif

#if ! defined ffoverflow || FFERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef FFSTACK_USE_ALLOCA
#  if FFSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define FFSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define FFSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define FFSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef FFSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define FFSTACK_FREE(Ptr) do { /* empty */; } while (FFID (0))
#  ifndef FFSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define FFSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define FFSTACK_ALLOC FFMALLOC
#  define FFSTACK_FREE FFFREE
#  ifndef FFSTACK_ALLOC_MAXIMUM
#   define FFSTACK_ALLOC_MAXIMUM FFSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined FFMALLOC || defined malloc) \
	     && (defined FFFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef FFMALLOC
#   define FFMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (FFSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef FFFREE
#   define FFFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined ffoverflow || FFERROR_VERBOSE */


#if (! defined ffoverflow \
     && (! defined __cplusplus \
	 || (defined FFSTYPE_IS_TRIVIAL && FFSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union ffalloc
{
  fftype_int16 ffss_alloc;
  FFSTYPE ffvs_alloc;
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
};
typedef enum ffsymbol_kind_t ffsymbol_kind_t;

<<<<<<< HEAD

=======
/* The size of the maximum gap between one aligned stack and the next.  */
# define FFSTACK_GAP_MAXIMUM (sizeof (union ffalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define FFSTACK_BYTES(N) \
     ((N) * (sizeof (fftype_int16) + sizeof (FFSTYPE)) \
      + FFSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef FFCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define FFCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define FFCOPY(To, From, Count)		\
      do					\
	{					\
	  FFSIZE_T ffi;				\
	  for (ffi = 0; ffi < (Count); ffi++)	\
	    (To)[ffi] = (From)[ffi];		\
	}					\
      while (FFID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables FFSIZE and FFSTACKSIZE give the old and new number of
   elements in the stack, and FFPTR gives the new location of the
   stack.  Advance FFPTR to a properly aligned location for the next
   stack.  */
# define FFSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	FFSIZE_T ffnewbytes;						\
	FFCOPY (&ffptr->Stack_alloc, Stack, ffsize);			\
	Stack = &ffptr->Stack_alloc;					\
	ffnewbytes = ffstacksize * sizeof (*Stack) + FFSTACK_GAP_MAXIMUM; \
	ffptr += ffnewbytes / sizeof (*ffptr);				\
      }									\
    while (FFID (0))
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0


#ifdef short
# undef short
#endif

<<<<<<< HEAD
/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define FF_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ fftype_int8;
#elif defined FF_STDINT_H
typedef int_least8_t fftype_int8;
#else
typedef signed char fftype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ fftype_int16;
#elif defined FF_STDINT_H
typedef int_least16_t fftype_int16;
#else
typedef short fftype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ fftype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined FF_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t fftype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char fftype_uint8;
#else
typedef short fftype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ fftype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined FF_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t fftype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short fftype_uint16;
#else
typedef int fftype_uint16;
#endif

#ifndef FFPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define FFPTRDIFF_T __PTRDIFF_TYPE__
#  define FFPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define FFPTRDIFF_T ptrdiff_t
#  define FFPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define FFPTRDIFF_T long
#  define FFPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef FFSIZE_T
# ifdef __SIZE_TYPE__
#  define FFSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define FFSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define FFSIZE_T size_t
# else
#  define FFSIZE_T unsigned
# endif
#endif

#define FFSIZE_MAXIMUM                                  \
  FF_CAST (FFPTRDIFF_T,                                 \
           (FFPTRDIFF_MAXIMUM < FF_CAST (FFSIZE_T, -1)  \
            ? FFPTRDIFF_MAXIMUM                         \
            : FF_CAST (FFSIZE_T, -1)))

#define FFSIZEOF(X) FF_CAST (FFPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef fftype_int16 ff_state_t;

/* State numbers in computations.  */
typedef int ff_state_fast_t;

#ifndef FF_
# if defined FFENABLE_NLS && FFENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define FF_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef FF_
#  define FF_(Msgid) Msgid
# endif
#endif


#ifndef FF_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define FF_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define FF_ATTRIBUTE_PURE
# endif
#endif

#ifndef FF_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define FF_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define FF_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define FFUSE(E) ((void) (E))
#else
# define FFUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about fflval being uninitialized.  */
# define FF_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define FF_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define FF_INITIAL_VALUE(Value) Value
#endif
#ifndef FF_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define FF_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define FF_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef FF_INITIAL_VALUE
# define FF_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define FF_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define FF_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef FF_IGNORE_USELESS_CAST_BEGIN
# define FF_IGNORE_USELESS_CAST_BEGIN
# define FF_IGNORE_USELESS_CAST_END
#endif


#define FF_ASSERT(E) ((void) (0 && (E)))

#if !defined ffoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef FFSTACK_USE_ALLOCA
#  if FFSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define FFSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define FFSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define FFSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef FFSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define FFSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef FFSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define FFSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define FFSTACK_ALLOC FFMALLOC
#  define FFSTACK_FREE FFFREE
#  ifndef FFSTACK_ALLOC_MAXIMUM
#   define FFSTACK_ALLOC_MAXIMUM FFSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined FFMALLOC || defined malloc) \
             && (defined FFFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef FFMALLOC
#   define FFMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (FFSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef FFFREE
#   define FFFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined ffoverflow */

#if (! defined ffoverflow \
     && (! defined __cplusplus \
         || (defined FFSTYPE_IS_TRIVIAL && FFSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union ffalloc
{
  ff_state_t ffss_alloc;
  FFSTYPE ffvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define FFSTACK_GAP_MAXIMUM (FFSIZEOF (union ffalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define FFSTACK_BYTES(N) \
     ((N) * (FFSIZEOF (ff_state_t) + FFSIZEOF (FFSTYPE)) \
      + FFSTACK_GAP_MAXIMUM)

# define FFCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables FFSIZE and FFSTACKSIZE give the old and new number of
   elements in the stack, and FFPTR gives the new location of the
   stack.  Advance FFPTR to a properly aligned location for the next
   stack.  */
# define FFSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        FFPTRDIFF_T ffnewbytes;                                         \
        FFCOPY (&ffptr->Stack_alloc, Stack, ffsize);                    \
        Stack = &ffptr->Stack_alloc;                                    \
        ffnewbytes = ffstacksize * FFSIZEOF (*Stack) + FFSTACK_GAP_MAXIMUM; \
        ffptr += ffnewbytes / FFSIZEOF (*ffptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined FFCOPY_NEEDED && FFCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef FFCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define FFCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, FF_CAST (FFSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define FFCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          FFPTRDIFF_T ffi;                      \
          for (ffi = 0; ffi < (Count); ffi++)   \
            (Dst)[ffi] = (Src)[ffi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !FFCOPY_NEEDED */

/* FFFINAL -- State number of the termination state.  */
#define FFFINAL  2
/* FFLAST -- Last index in FFTABLE.  */
#define FFLAST   1725

/* FFNTOKENS -- Number of terminals.  */
#define FFNTOKENS  56
/* FFNNTS -- Number of nonterminals.  */
#define FFNNTS  9
/* FFNRULES -- Number of rules.  */
#define FFNRULES  130
/* FFNSTATES -- Number of states.  */
#define FFNSTATES  308

/* FFMAXUTOK -- Last valid token kind.  */
#define FFMAXUTOK   291


/* FFTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by fflex, with out-of-bounds checking.  */
#define FFTRANSLATE(FFX)                                \
  (0 <= (FFX) && (FFX) <= FFMAXUTOK                     \
   ? FF_CAST (ffsymbol_kind_t, fftranslate[FFX])        \
   : FFSYMBOL_FFUNDEF)

/* FFTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by fflex.  */
static const fftype_int8 fftranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      52,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    38,    42,     2,
      54,    55,    39,    36,    21,    37,     2,    40,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    23,     2,
       2,    22,     2,    26,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    49,     2,    53,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    24,    41,    25,    31,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    27,    28,    29,    30,
      32,    33,    34,    35,    43,    44,    45,    46,    47,    48,
      50,    51
};

#if FFDEBUG
  /* FFRLINE[FFN] -- Source line where rule number FFN was defined.  */
static const fftype_int16 ffrline[] =
{
       0,   252,   252,   253,   256,   257,   263,   269,   275,   281,
     284,   286,   299,   301,   314,   325,   339,   343,   347,   351,
     353,   362,   365,   368,   377,   379,   381,   383,   385,   387,
     390,   394,   396,   398,   400,   409,   411,   413,   416,   419,
     422,   425,   428,   437,   446,   455,   458,   460,   462,   464,
     468,   472,   491,   510,   529,   540,   554,   566,   597,   692,
     700,   761,   785,   787,   789,   791,   793,   795,   797,   799,
     801,   805,   807,   809,   818,   821,   824,   827,   830,   833,
     836,   839,   842,   845,   848,   851,   854,   857,   860,   863,
     866,   869,   872,   875,   877,   879,   881,   884,   891,   908,
     921,   934,   945,   961,   985,  1013,  1050,  1054,  1058,  1061,
    1066,  1069,  1074,  1078,  1081,  1085,  1087,  1089,  1091,  1093,
    1095,  1097,  1101,  1104,  1106,  1115,  1117,  1119,  1128,  1147,
    1166
};
#endif

/** Accessing symbol of state STATE.  */
#define FF_ACCESSING_SYMBOL(State) FF_CAST (ffsymbol_kind_t, ffstos[State])

#if FFDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   FFSYMBOL.  No bounds checking.  */
static const char *ffsymbol_name (ffsymbol_kind_t ffsymbol) FF_ATTRIBUTE_UNUSED;

/* FFTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at FFNTOKENS, nonterminals.  */
static const char *const fftname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "BOOLEAN", "LONG",
  "DOUBLE", "STRING", "BITSTR", "FUNCTION", "BFUNCTION", "IFUNCTION",
  "GTIFILTER", "GTIOVERLAP", "REGFILTER", "COLUMN", "BCOLUMN", "SCOLUMN",
  "BITCOL", "ROWREF", "NULLREF", "SNULLREF", "','", "'='", "':'", "'{'",
  "'}'", "'?'", "OR", "AND", "EQ", "NE", "'~'", "GT", "LT", "LTE", "GTE",
  "'+'", "'-'", "'%'", "'*'", "'/'", "'|'", "'&'", "XOR", "POWER", "NOT",
  "INTCAST", "FLTCAST", "UMINUS", "'['", "ACCUM", "DIFF", "'\\n'", "']'",
  "'('", "')'", "$accept", "lines", "line", "bvector", "vector", "expr",
  "bexpr", "bits", "sexpr", FF_NULLPTR
};

static const char *
ffsymbol_name (ffsymbol_kind_t ffsymbol)
{
  return fftname[ffsymbol];
}
#endif

#ifdef FFPRINT
/* FFTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const fftype_int16 fftoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,    44,    61,    58,   123,   125,    63,   276,   277,   278,
     279,   126,   280,   281,   282,   283,    43,    45,    37,    42,
      47,   124,    38,   284,   285,   286,   287,   288,   289,    91,
     290,   291,    10,    93,    40,    41
};
#endif

#define FFPACT_NINF (-40)

#define ffpact_value_is_default(Yyn) \
  ((Yyn) == FFPACT_NINF)

#define FFTABLE_NINF (-1)

#define fftable_value_is_error(Yyn) \
  0

  /* FFPACT[STATE-NUM] -- Index in FFTABLE of the portion describing
     STATE-NUM.  */
static const fftype_int16 ffpact[] =
{
     -40,   337,   -40,   -39,   -40,   -40,   -40,   -40,   -40,   389,
     442,   442,    -5,    21,    29,    17,    25,    44,    45,   -40,
     -40,   -40,   442,   442,   442,   442,   442,   442,   -40,   442,
     -40,   -15,    19,  1159,   443,  1584,  1605,   -40,   -40,   276,
     -10,   330,   133,   469,   144,  1647,   248,  1526,   209,  1689,
     -19,   -40,    49,   -18,   442,   442,   442,   442,  1526,   209,
     294,    -6,    -6,    24,    26,    -6,    24,    -6,    24,   671,
    1186,   382,  1544,   442,   -40,   442,   -40,   442,   442,   442,
     442,   442,   442,   442,   442,   442,   442,   442,   442,   442,
     442,   442,   442,   442,   442,   -40,   442,   442,   442,   442,
     442,   442,   442,   -40,    -3,    -3,    -3,    -3,    -3,    -3,
      -3,    -3,    -3,   442,   -40,   442,   442,   442,   442,   442,
     442,   442,   -40,   442,   -40,   442,   -40,   -40,   442,   -40,
     442,   -40,   -40,   -40,   442,   442,   -40,   442,   442,   -40,
    1388,  1411,  1434,  1457,   -40,   -40,   -40,   -40,  1526,   209,
    1526,   209,  1480,  1665,  1665,  1665,    22,    22,    22,    22,
     203,   203,   203,   148,    24,   148,   -37,   -37,   -37,   -37,
     784,  1503,  1558,  1619,    18,    69,   -34,   -34,   148,   809,
      -3,    -3,   111,   111,   111,   111,   111,   111,   -11,    26,
      26,   834,   406,   406,    58,    58,    58,    58,   -40,   498,
    1191,  1221,  1560,  1245,  1576,   527,  1269,  1293,   -40,   -40,
     -40,   -40,   442,   442,   -40,   442,   442,   442,   442,   -40,
      26,    68,   442,   -40,   442,   -40,   -40,   442,   -40,   442,
     -40,    90,   -40,   442,   442,  1629,   859,  1629,   209,  1629,
     209,   294,   884,   909,  1317,   699,   556,    76,   585,   614,
     442,   -40,   442,   -40,   442,   -40,   442,   -40,   442,   -40,
      96,    97,   -40,    99,   -40,   934,   959,   984,   727,  1341,
      51,    92,    56,   442,   -40,   442,   -40,   442,   -40,   -40,
     442,   -40,   108,   -40,  1009,  1034,  1059,   643,    65,   442,
     -40,   442,   -40,   442,   -40,   442,   -40,   -40,  1084,  1109,
    1134,  1365,   -40,   -40,   -40,   442,   755,   -40
};

  /* FFDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when FFTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const fftype_uint8 ffdefact[] =
{
       2,     0,     1,     0,    71,    31,    32,   122,    18,     0,
       0,     0,     0,     0,     0,    33,    72,   123,    19,    35,
      36,   125,     0,     0,     0,     0,     0,     0,     4,     0,
       3,     0,     0,     0,     0,     0,     0,     9,    54,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,    12,    10,
       0,    46,    47,   120,    29,    67,    68,    69,    70,     0,
       0,     0,     0,     0,    17,     0,    16,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     0,     0,     0,     0,
       0,     0,     0,     6,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     8,     0,     0,     0,     0,     0,
       0,     0,     7,     0,    58,     0,    55,    57,     0,    56,
       0,    99,   100,   101,     0,     0,   107,     0,     0,   112,
       0,     0,     0,     0,    48,   121,    30,   126,    15,    11,
      13,    14,     0,    85,    86,    84,    80,    81,    83,    82,
      38,    39,    37,    40,    49,    41,    43,    42,    44,    45,
       0,     0,     0,     0,    94,    93,    95,    96,    50,     0,
       0,     0,    74,    75,    78,    76,    77,    79,    23,    22,
      21,     0,    87,    88,    89,    91,    92,    90,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    34,    73,
     124,    20,     0,     0,    62,     0,     0,     0,     0,   115,
      29,     0,     0,    24,     0,    60,   102,     0,   129,     0,
      59,     0,   108,     0,     0,    97,     0,    51,    53,    52,
      98,   128,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,     0,   116,     0,    25,     0,   130,     0,   103,
       0,     0,   110,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,   117,     0,    26,    61,
       0,   109,     0,   114,     0,     0,     0,     0,     0,     0,
      65,     0,   118,     0,    27,     0,   104,   111,     0,     0,
       0,     0,    66,   119,    28,     0,     0,   105
};

  /* FFPGOTO[NTERM-NUM].  */
static const fftype_int16 ffpgoto[] =
{
     -40,   -40,   -40,   -40,   -40,    -1,   106,   155,    23
};

  /* FFDEFGOTO[NTERM-NUM].  */
static const fftype_int8 ffdefgoto[] =
{
      -1,     1,    30,    31,    32,    47,    48,    45,    60
};

  /* FFTABLE[FFPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If FFTABLE_NINF, syntax error.  */
static const fftype_int16 fftable[] =
{
      33,    50,   135,   138,     8,   101,    73,    93,    39,    43,
      74,   125,    94,    37,    18,   102,    96,    97,    98,    99,
     100,    58,    61,    62,    36,    65,    67,    52,    69,   101,
     111,   112,    42,    46,    49,    53,   136,   139,   113,   102,
      75,    54,   180,    94,    76,   126,    98,    99,   100,    55,
      51,   181,    72,   140,   141,   142,   143,   101,    85,    86,
      87,    88,    89,    90,    91,    92,    93,   102,    56,    57,
     137,    94,   148,   102,   150,   113,   152,   153,   154,   155,
     156,   157,   158,   159,   160,   161,   162,   163,   165,   166,
     167,   168,   169,   170,   121,   171,   247,   260,    99,   100,
     178,   179,   270,   271,   110,   272,   281,    34,   101,   111,
     112,   283,   191,   282,   288,    40,    44,   113,   102,   173,
     297,     0,   199,   146,     0,     0,     0,   201,    59,   203,
       0,    63,    66,    68,   205,    70,   206,   207,   192,   193,
     194,   195,   196,   197,   198,     0,     0,   110,     0,     0,
       0,   202,   111,   112,   128,     0,    35,   204,     0,     0,
     113,     0,   115,   116,    41,   117,   118,   119,   120,   121,
      96,    97,    98,    99,   100,     0,     0,     0,     0,   149,
      64,   151,     0,   101,    71,     0,     0,     0,   129,    90,
      91,    92,    93,   102,   164,     0,     0,    94,     0,   132,
       0,     0,   172,   174,   175,   176,   177,     0,     0,     0,
       0,   235,   236,     0,   237,   239,     0,   242,     0,     0,
       0,   243,     0,   244,     0,     0,   245,     0,   246,     0,
       0,   200,   248,   249,     0,    96,    97,    98,    99,   100,
     241,     0,    88,    89,    90,    91,    92,    93,   101,   265,
       0,   266,    94,   267,     0,   268,     0,   269,   102,   182,
     183,   184,   185,   186,   187,   188,   189,   190,     0,     0,
       0,     0,   284,     0,   285,     0,   286,   115,   116,   287,
     117,   118,   119,   120,   121,     0,     0,     0,   298,     0,
     299,     0,   300,     0,   301,     0,     0,   123,    77,     0,
       0,     0,     0,   133,   306,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,   238,   240,   115,   116,    94,   117,   118,   119,   120,
     121,   124,     0,     0,     0,   220,   221,     2,     3,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,     0,   104,
     105,    22,   106,   107,   108,   109,   110,     0,     0,     0,
       0,   111,   112,    23,    24,     0,     0,     0,     0,   113,
       0,     0,    25,    26,    27,   127,     0,     0,     0,    28,
       0,    29,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
       0,   104,   105,    22,   106,   107,   108,   109,   110,     0,
       0,     0,     0,   111,   112,    23,    24,     0,     0,     0,
       0,   113,     0,     0,    25,    26,    27,   146,   117,   118,
     119,   120,   121,    29,    38,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,     0,     0,     0,    22,     0,     0,    96,
      97,    98,    99,   100,     0,     0,     0,     0,    23,    24,
       0,     0,   101,     0,     0,     0,     0,    25,    26,    27,
     130,    77,   102,     0,     0,   103,    29,     0,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,     0,     0,     0,    94,   224,
      77,     0,     0,     0,   131,     0,     0,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,     0,     0,     0,     0,    94,   231,    77,
       0,     0,     0,   225,     0,     0,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,     0,     0,     0,     0,    94,   258,    77,     0,
       0,     0,   232,     0,     0,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,     0,     0,     0,     0,    94,   261,    77,     0,     0,
       0,   259,     0,     0,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
       0,     0,     0,     0,    94,   263,    77,     0,     0,     0,
     262,     0,     0,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
       0,     0,     0,    94,   295,    77,     0,     0,     0,   264,
       0,     0,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,     0,     0,
       0,     0,    94,    77,     0,     0,     0,     0,   296,     0,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,     0,     0,     0,     0,
      94,    77,     0,     0,     0,     0,   144,     0,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,     0,     0,     0,    94,    77,
       0,     0,     0,     0,   257,     0,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,     0,     0,     0,     0,    94,    77,     0,     0,
       0,     0,   279,     0,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
       0,     0,     0,     0,    94,   213,    77,     0,     0,     0,
     307,     0,     0,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
     218,    77,     0,    94,     0,     0,     0,   214,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,   222,    77,     0,    94,     0,
       0,     0,   219,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
     250,    77,     0,    94,     0,     0,     0,   223,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,   252,    77,     0,    94,     0,
       0,     0,   251,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
     254,    77,     0,    94,     0,     0,     0,   253,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,   273,    77,     0,    94,     0,
       0,     0,   255,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
     275,    77,     0,    94,     0,     0,     0,   274,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,   277,    77,     0,    94,     0,
       0,     0,   276,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
     289,    77,     0,    94,     0,     0,     0,   278,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,   291,    77,     0,    94,     0,
       0,     0,   290,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
     293,    77,     0,    94,     0,     0,     0,   292,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,     0,    77,     0,    94,     0,
       0,     0,   294,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
       0,    77,     0,    94,     0,     0,     0,   302,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,     0,    77,     0,    94,     0,
       0,     0,   303,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,     0,
       0,    77,     0,    94,     0,     0,     0,   304,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,     0,     0,     0,    94,     0,
       0,    95,    96,    97,    98,    99,   100,    96,    97,    98,
      99,   100,     0,     0,     0,   101,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,     0,     0,     0,     0,
     102,   145,   227,    77,     0,     0,   226,     0,     0,     0,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,   229,    77,     0,     0,
      94,     0,     0,     0,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
     233,    77,     0,     0,    94,     0,     0,     0,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,   234,    77,     0,     0,    94,     0,
       0,     0,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,   256,    77,
       0,     0,    94,     0,     0,     0,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,   280,    77,     0,     0,    94,     0,     0,     0,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,   305,    77,     0,     0,
      94,     0,     0,     0,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      77,     0,     0,   208,    94,     0,     0,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    77,     0,     0,   209,    94,     0,     0,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    77,     0,     0,   210,
      94,     0,     0,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    77,
       0,     0,   211,    94,     0,     0,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    77,   212,     0,     0,    94,     0,     0,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    77,   215,     0,     0,    94,
       0,     0,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    77,     0,
       0,     0,    94,     0,     0,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,     0,     0,   115,   116,    94,   117,   118,   119,   120,
     121,   216,     0,     0,    96,    97,    98,    99,   100,   115,
     116,     0,   117,   118,   119,   120,   121,   101,     0,   147,
       0,     0,     0,     0,     0,   115,   116,   102,   117,   118,
     119,   120,   121,   104,   105,   228,   106,   107,   108,   109,
     110,     0,     0,     0,     0,   111,   112,     0,     0,     0,
       0,   230,     0,   113,   115,   116,   114,   117,   118,   119,
     120,   121,   217,     0,     0,     0,     0,     0,   115,   116,
       0,   117,   118,   119,   120,   121,     0,   122,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,     0,     0,   104,   105,    94,   106,
     107,   108,   109,   110,     0,     0,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,   113,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
     134,     0,     0,     0,    94,     0,     0,     0,   115,   116,
       0,   117,   118,   119,   120,   121
};

static const fftype_int16 ffcheck[] =
{
       1,     6,    21,    21,     7,    39,    21,    44,     9,    10,
      25,    21,    49,    52,    17,    49,    26,    27,    28,    29,
      30,    22,    23,    24,     1,    26,    27,     6,    29,    39,
      41,    42,     9,    10,    11,     6,    55,    55,    49,    49,
      21,    24,    45,    49,    25,    55,    28,    29,    30,    24,
      55,    54,    29,    54,    55,    56,    57,    39,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    49,    24,    24,
      21,    49,    73,    49,    75,    49,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    36,    96,     6,    21,    29,    30,
     101,   102,     6,     6,    36,     6,    55,     1,    39,    41,
      42,    55,   113,    21,     6,     9,    10,    49,    49,    96,
      55,    -1,   123,    55,    -1,    -1,    -1,   128,    22,   130,
      -1,    25,    26,    27,   135,    29,   137,   138,   115,   116,
     117,   118,   119,   120,   121,    -1,    -1,    36,    -1,    -1,
      -1,   128,    41,    42,    21,    -1,     1,   134,    -1,    -1,
      49,    -1,    29,    30,     9,    32,    33,    34,    35,    36,
      26,    27,    28,    29,    30,    -1,    -1,    -1,    -1,    73,
      25,    75,    -1,    39,    29,    -1,    -1,    -1,    55,    41,
      42,    43,    44,    49,    88,    -1,    -1,    49,    -1,    55,
      -1,    -1,    96,    97,    98,    99,   100,    -1,    -1,    -1,
      -1,   212,   213,    -1,   215,   216,    -1,   218,    -1,    -1,
      -1,   222,    -1,   224,    -1,    -1,   227,    -1,   229,    -1,
      -1,   125,   233,   234,    -1,    26,    27,    28,    29,    30,
     217,    -1,    39,    40,    41,    42,    43,    44,    39,   250,
      -1,   252,    49,   254,    -1,   256,    -1,   258,    49,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,   273,    -1,   275,    -1,   277,    29,    30,   280,
      32,    33,    34,    35,    36,    -1,    -1,    -1,   289,    -1,
     291,    -1,   293,    -1,   295,    -1,    -1,    21,    22,    -1,
      -1,    -1,    -1,    55,   305,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,   215,   216,    29,    30,    49,    32,    33,    34,    35,
      36,    55,    -1,    -1,    -1,   180,   181,     0,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    -1,    29,
      30,    24,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      -1,    41,    42,    36,    37,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    45,    46,    47,    55,    -1,    -1,    -1,    52,
      -1,    54,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      -1,    29,    30,    24,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    -1,    41,    42,    36,    37,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    45,    46,    47,    55,    32,    33,
      34,    35,    36,    54,    55,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    -1,    -1,    -1,    24,    -1,    -1,    26,
      27,    28,    29,    30,    -1,    -1,    -1,    -1,    36,    37,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    45,    46,    47,
      21,    22,    49,    -1,    -1,    52,    54,    -1,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    49,    21,
      22,    -1,    -1,    -1,    55,    -1,    -1,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    49,    21,    22,
      -1,    -1,    -1,    55,    -1,    -1,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    49,    21,    22,    -1,
      -1,    -1,    55,    -1,    -1,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    49,    21,    22,    -1,    -1,
      -1,    55,    -1,    -1,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    49,    21,    22,    -1,    -1,    -1,
      55,    -1,    -1,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    49,    21,    22,    -1,    -1,    -1,    55,
      -1,    -1,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    49,    22,    -1,    -1,    -1,    -1,    55,    -1,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      49,    22,    -1,    -1,    -1,    -1,    55,    -1,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    49,    22,
      -1,    -1,    -1,    -1,    55,    -1,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    49,    22,    -1,    -1,
      -1,    -1,    55,    -1,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    49,    21,    22,    -1,    -1,    -1,
      55,    -1,    -1,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      21,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    21,    22,    -1,    49,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      21,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    21,    22,    -1,    49,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      21,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    21,    22,    -1,    49,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      21,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    21,    22,    -1,    49,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      21,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    21,    22,    -1,    49,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      21,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    22,    -1,    49,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    22,    -1,    49,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    22,    -1,    49,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    26,    27,    28,    29,    30,    26,    27,    28,
      29,    30,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,    -1,
      49,    55,    21,    22,    -1,    -1,    55,    -1,    -1,    -1,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    21,    22,    -1,    -1,
      49,    -1,    -1,    -1,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      21,    22,    -1,    -1,    49,    -1,    -1,    -1,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    21,    22,    -1,    -1,    49,    -1,
      -1,    -1,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    21,    22,
      -1,    -1,    49,    -1,    -1,    -1,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    21,    22,    -1,    -1,    49,    -1,    -1,    -1,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    21,    22,    -1,    -1,
      49,    -1,    -1,    -1,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      22,    -1,    -1,    25,    49,    -1,    -1,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    22,    -1,    -1,    25,    49,    -1,    -1,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    22,    -1,    -1,    25,
      49,    -1,    -1,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    22,
      -1,    -1,    25,    49,    -1,    -1,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    22,    23,    -1,    -1,    49,    -1,    -1,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    22,    23,    -1,    -1,    49,
      -1,    -1,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    22,    -1,
      -1,    -1,    49,    -1,    -1,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    29,    30,    49,    32,    33,    34,    35,
      36,    23,    -1,    -1,    26,    27,    28,    29,    30,    29,
      30,    -1,    32,    33,    34,    35,    36,    39,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    29,    30,    49,    32,    33,
      34,    35,    36,    29,    30,    55,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    -1,    41,    42,    -1,    -1,    -1,
      -1,    55,    -1,    49,    29,    30,    52,    32,    33,    34,
      35,    36,    23,    -1,    -1,    -1,    -1,    -1,    29,    30,
      -1,    32,    33,    34,    35,    36,    -1,    52,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    29,    30,    49,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    -1,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      21,    -1,    -1,    -1,    49,    -1,    -1,    -1,    29,    30,
      -1,    32,    33,    34,    35,    36
};

  /* FFSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const fftype_int8 ffstos[] =
{
       0,    57,     0,     1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    24,    36,    37,    45,    46,    47,    52,    54,
      58,    59,    60,    61,    62,    63,    64,    52,    55,    61,
      62,    63,    64,    61,    62,    63,    64,    61,    62,    64,
       6,    55,     6,     6,    24,    24,    24,    24,    61,    62,
      64,    61,    61,    62,    63,    61,    62,    61,    62,    61,
      62,    63,    64,    21,    25,    21,    25,    22,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    49,    52,    26,    27,    28,    29,
      30,    39,    49,    52,    29,    30,    32,    33,    34,    35,
      36,    41,    42,    49,    52,    29,    30,    32,    33,    34,
      35,    36,    52,    21,    55,    21,    55,    55,    21,    55,
      21,    55,    55,    55,    21,    21,    55,    21,    21,    55,
      61,    61,    61,    61,    55,    55,    55,    55,    61,    62,
      61,    62,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    62,    61,    61,    61,    61,    61,
      61,    61,    62,    64,    62,    62,    62,    62,    61,    61,
      45,    54,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    61,    64,    64,    64,    64,    64,    64,    64,    61,
      62,    61,    64,    61,    64,    61,    61,    61,    25,    25,
      25,    25,    23,    21,    53,    23,    23,    23,    21,    53,
      63,    63,    21,    53,    21,    55,    55,    21,    55,    21,
      55,    21,    55,    21,    21,    61,    61,    61,    62,    61,
      62,    64,    61,    61,    61,    61,    61,     6,    61,    61,
      21,    53,    21,    53,    21,    53,    21,    55,    21,    55,
      21,    21,    55,    21,    55,    61,    61,    61,    61,    61,
       6,     6,     6,    21,    53,    21,    53,    21,    53,    55,
      21,    55,    21,    55,    61,    61,    61,    61,     6,    21,
      53,    21,    53,    21,    53,    21,    55,    55,    61,    61,
      61,    61,    53,    53,    53,    21,    61,    55
};

  /* FFR1[FFN] -- Symbol number of symbol that rule FFN derives.  */
static const fftype_int8 ffr1[] =
{
       0,    56,    57,    57,    58,    58,    58,    58,    58,    58,
      59,    59,    60,    60,    60,    60,    61,    62,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    64,    64,    64,    64,    64,    64,    64,    64,
      64
};

  /* FFR2[FFN] -- Number of symbols on the right hand side of rule FFN.  */
static const fftype_int8 ffr2[] =
{
       0,     2,     0,     2,     1,     2,     2,     2,     2,     2,
       2,     3,     2,     3,     3,     3,     2,     2,     1,     1,
       4,     3,     3,     3,     4,     6,     8,    10,    12,     2,
       3,     1,     1,     1,     4,     1,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     2,     3,     3,
       3,     5,     5,     5,     2,     3,     3,     3,     3,     5,
       5,     9,     4,     6,     8,    10,    12,     2,     2,     2,
       2,     1,     1,     4,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     5,     5,     3,
       3,     3,     5,     7,    11,    15,     2,     3,     5,     9,
       7,    11,     3,     7,     9,     4,     6,     8,    10,    12,
       2,     3,     1,     1,     4,     1,     3,     3,     5,     5,
       7
};


enum { FFENOMEM = -2 };

#define fferrok         (fferrstatus = 0)
#define ffclearin       (ffchar = FFEMPTY)

#define FFACCEPT        goto ffacceptlab
#define FFABORT         goto ffabortlab
#define FFERROR         goto fferrorlab


#define FFRECOVERING()  (!!fferrstatus)
=======
/* FFFINAL -- State number of the termination state.  */
#define FFFINAL  2
/* FFLAST -- Last index in FFTABLE.  */
#define FFLAST   1603

/* FFNTOKENS -- Number of terminals.  */
#define FFNTOKENS  54
/* FFNNTS -- Number of nonterminals.  */
#define FFNNTS  9
/* FFNRULES -- Number of rules.  */
#define FFNRULES  125
/* FFNRULES -- Number of states.  */
#define FFNSTATES  290

/* FFTRANSLATE(FFLEX) -- Bison symbol number corresponding to FFLEX.  */
#define FFUNDEFTOK  2
#define FFMAXUTOK   289

#define FFTRANSLATE(FFX)						\
  ((unsigned int) (FFX) <= FFMAXUTOK ? fftranslate[FFX] : FFUNDEFTOK)

/* FFTRANSLATE[FFLEX] -- Bison symbol number corresponding to FFLEX.  */
static const fftype_uint8 fftranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      50,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    37,    41,     2,
      52,    53,    38,    35,    20,    36,     2,    39,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    22,     2,
       2,    21,     2,    25,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    47,     2,    51,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    23,    40,    24,    28,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    26,    27,    29,    30,    31,
      32,    33,    34,    42,    43,    44,    45,    46,    48,    49
};

#if FFDEBUG
/* FFPRHS[FFN] -- Index of the first RHS symbol of rule number FFN in
   FFRHS.  */
static const fftype_uint16 ffprhs[] =
{
       0,     0,     3,     4,     7,     9,    12,    15,    18,    21,
      24,    27,    31,    34,    38,    42,    46,    49,    52,    54,
      56,    61,    65,    69,    73,    78,    85,    94,   105,   118,
     121,   125,   127,   129,   131,   136,   138,   140,   144,   148,
     152,   156,   160,   164,   167,   170,   174,   178,   182,   188,
     194,   200,   203,   207,   211,   215,   219,   225,   231,   241,
     246,   253,   262,   273,   286,   289,   292,   295,   298,   300,
     302,   307,   311,   315,   319,   323,   327,   331,   335,   339,
     343,   347,   351,   355,   359,   363,   367,   371,   375,   379,
     383,   387,   391,   395,   399,   405,   411,   415,   419,   423,
     429,   437,   449,   465,   468,   472,   478,   488,   492,   500,
     510,   515,   522,   531,   542,   555,   558,   562,   564,   566,
     571,   573,   577,   581,   587,   593
};

/* FFRHS -- A `-1'-separated list of the rules' RHS.  */
static const fftype_int8 ffrhs[] =
{
      55,     0,    -1,    -1,    55,    56,    -1,    50,    -1,    59,
      50,    -1,    60,    50,    -1,    62,    50,    -1,    61,    50,
      -1,     1,    50,    -1,    23,    60,    -1,    57,    20,    60,
      -1,    23,    59,    -1,    58,    20,    59,    -1,    58,    20,
      60,    -1,    57,    20,    59,    -1,    58,    24,    -1,    57,
      24,    -1,     7,    -1,    16,    -1,    16,    23,    59,    24,
      -1,    61,    41,    61,    -1,    61,    40,    61,    -1,    61,
      35,    61,    -1,    61,    47,    59,    51,    -1,    61,    47,
      59,    20,    59,    51,    -1,    61,    47,    59,    20,    59,
      20,    59,    51,    -1,    61,    47,    59,    20,    59,    20,
      59,    20,    59,    51,    -1,    61,    47,    59,    20,    59,
      20,    59,    20,    59,    20,    59,    51,    -1,    43,    61,
      -1,    52,    61,    53,    -1,     4,    -1,     5,    -1,    13,
      -1,    13,    23,    59,    24,    -1,    17,    -1,    18,    -1,
      59,    37,    59,    -1,    59,    35,    59,    -1,    59,    36,
      59,    -1,    59,    38,    59,    -1,    59,    39,    59,    -1,
      59,    42,    59,    -1,    35,    59,    -1,    36,    59,    -1,
      52,    59,    53,    -1,    59,    38,    60,    -1,    60,    38,
      59,    -1,    60,    25,    59,    22,    59,    -1,    60,    25,
      60,    22,    59,    -1,    60,    25,    59,    22,    60,    -1,
       8,    53,    -1,     8,    60,    53,    -1,     8,    62,    53,
      -1,     8,    61,    53,    -1,     8,    59,    53,    -1,    10,
      62,    20,    62,    53,    -1,     8,    59,    20,    59,    53,
      -1,     8,    59,    20,    59,    20,    59,    20,    59,    53,
      -1,    59,    47,    59,    51,    -1,    59,    47,    59,    20,
      59,    51,    -1,    59,    47,    59,    20,    59,    20,    59,
      51,    -1,    59,    47,    59,    20,    59,    20,    59,    20,
      59,    51,    -1,    59,    47,    59,    20,    59,    20,    59,
      20,    59,    20,    59,    51,    -1,    45,    59,    -1,    45,
      60,    -1,    44,    59,    -1,    44,    60,    -1,     3,    -1,
      14,    -1,    14,    23,    59,    24,    -1,    61,    30,    61,
      -1,    61,    29,    61,    -1,    61,    33,    61,    -1,    61,
      32,    61,    -1,    61,    34,    61,    -1,    61,    31,    61,
      -1,    59,    34,    59,    -1,    59,    33,    59,    -1,    59,
      31,    59,    -1,    59,    32,    59,    -1,    59,    28,    59,
      -1,    59,    30,    59,    -1,    59,    29,    59,    -1,    62,
      30,    62,    -1,    62,    29,    62,    -1,    62,    34,    62,
      -1,    62,    31,    62,    -1,    62,    33,    62,    -1,    62,
      32,    62,    -1,    60,    27,    60,    -1,    60,    26,    60,
      -1,    60,    30,    60,    -1,    60,    29,    60,    -1,    59,
      21,    59,    22,    59,    -1,    60,    25,    60,    22,    60,
      -1,     9,    59,    53,    -1,     9,    60,    53,    -1,     9,
      62,    53,    -1,     8,    60,    20,    60,    53,    -1,     9,
      59,    20,    59,    20,    59,    53,    -1,     9,    59,    20,
      59,    20,    59,    20,    59,    20,    59,    53,    -1,     9,
      59,    20,    59,    20,    59,    20,    59,    20,    59,    20,
      59,    20,    59,    53,    -1,    11,    53,    -1,    11,     6,
      53,    -1,    11,     6,    20,    59,    53,    -1,    11,     6,
      20,    59,    20,     6,    20,     6,    53,    -1,    12,     6,
      53,    -1,    12,     6,    20,    59,    20,    59,    53,    -1,
      12,     6,    20,    59,    20,    59,    20,     6,    53,    -1,
      60,    47,    59,    51,    -1,    60,    47,    59,    20,    59,
      51,    -1,    60,    47,    59,    20,    59,    20,    59,    51,
      -1,    60,    47,    59,    20,    59,    20,    59,    20,    59,
      51,    -1,    60,    47,    59,    20,    59,    20,    59,    20,
      59,    20,    59,    51,    -1,    43,    60,    -1,    52,    60,
      53,    -1,     6,    -1,    15,    -1,    15,    23,    59,    24,
      -1,    19,    -1,    52,    62,    53,    -1,    62,    35,    62,
      -1,    60,    25,    62,    22,    62,    -1,     8,    62,    20,
      62,    53,    -1,     8,    62,    20,    59,    20,    59,    53,
      -1
};

/* FFRLINE[FFN] -- source line where rule number FFN was defined.  */
static const fftype_uint16 ffrline[] =
{
       0,   244,   244,   245,   248,   249,   255,   261,   267,   273,
     276,   278,   291,   293,   306,   317,   331,   335,   339,   343,
     345,   354,   357,   360,   369,   371,   373,   375,   377,   379,
     382,   386,   388,   390,   392,   401,   403,   405,   408,   411,
     414,   417,   420,   423,   425,   427,   429,   433,   437,   456,
     475,   494,   505,   519,   531,   562,   657,   665,   727,   751,
     753,   755,   757,   759,   761,   763,   765,   767,   771,   773,
     775,   784,   787,   790,   793,   796,   799,   802,   805,   808,
     811,   814,   817,   820,   823,   826,   829,   832,   835,   838,
     841,   843,   845,   847,   850,   857,   874,   887,   900,   911,
     927,   951,   979,  1016,  1020,  1024,  1027,  1031,  1035,  1038,
    1042,  1044,  1046,  1048,  1050,  1052,  1054,  1058,  1061,  1063,
    1072,  1074,  1076,  1085,  1104,  1123
};
#endif

#if FFDEBUG || FFERROR_VERBOSE || FFTOKEN_TABLE
/* FFTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at FFNTOKENS, nonterminals.  */
static const char *const fftname[] =
{
  "$end", "error", "$undefined", "BOOLEAN", "LONG", "DOUBLE", "STRING",
  "BITSTR", "FUNCTION", "BFUNCTION", "IFUNCTION", "GTIFILTER", "REGFILTER",
  "COLUMN", "BCOLUMN", "SCOLUMN", "BITCOL", "ROWREF", "NULLREF",
  "SNULLREF", "','", "'='", "':'", "'{'", "'}'", "'?'", "OR", "AND", "'~'",
  "NE", "EQ", "GTE", "LTE", "LT", "GT", "'+'", "'-'", "'%'", "'*'", "'/'",
  "'|'", "'&'", "POWER", "NOT", "FLTCAST", "INTCAST", "UMINUS", "'['",
  "DIFF", "ACCUM", "'\\n'", "']'", "'('", "')'", "$accept", "lines",
  "line", "bvector", "vector", "expr", "bexpr", "bits", "sexpr", 0
};
#endif

# ifdef FFPRINT
/* FFTOKNUM[FFLEX-NUM] -- Internal token number corresponding to
   token FFLEX-NUM.  */
static const fftype_uint16 fftoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
      44,    61,    58,   123,   125,    63,   275,   276,   126,   277,
     278,   279,   280,   281,   282,    43,    45,    37,    42,    47,
     124,    38,   283,   284,   285,   286,   287,    91,   288,   289,
      10,    93,    40,    41
};
# endif

/* FFR1[FFN] -- Symbol number of symbol that rule FFN derives.  */
static const fftype_uint8 ffr1[] =
{
       0,    54,    55,    55,    56,    56,    56,    56,    56,    56,
      57,    57,    58,    58,    58,    58,    59,    60,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    59,    59,    59,    59,    59,    59,    59,    59,    59,
      59,    59,    59,    59,    59,    59,    59,    59,    59,    59,
      59,    59,    59,    59,    59,    59,    59,    59,    59,    59,
      59,    59,    59,    59,    59,    59,    59,    59,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    62,    62,    62,
      62,    62,    62,    62,    62,    62
};

/* FFR2[FFN] -- Number of symbols composing right hand side of rule FFN.  */
static const fftype_uint8 ffr2[] =
{
       0,     2,     0,     2,     1,     2,     2,     2,     2,     2,
       2,     3,     2,     3,     3,     3,     2,     2,     1,     1,
       4,     3,     3,     3,     4,     6,     8,    10,    12,     2,
       3,     1,     1,     1,     4,     1,     1,     3,     3,     3,
       3,     3,     3,     2,     2,     3,     3,     3,     5,     5,
       5,     2,     3,     3,     3,     3,     5,     5,     9,     4,
       6,     8,    10,    12,     2,     2,     2,     2,     1,     1,
       4,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     5,     5,     3,     3,     3,     5,
       7,    11,    15,     2,     3,     5,     9,     3,     7,     9,
       4,     6,     8,    10,    12,     2,     3,     1,     1,     4,
       1,     3,     3,     5,     5,     7
};

/* FFDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when FFTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const fftype_uint8 ffdefact[] =
{
       2,     0,     1,     0,    68,    31,    32,   117,    18,     0,
       0,     0,     0,     0,    33,    69,   118,    19,    35,    36,
     120,     0,     0,     0,     0,     0,     0,     4,     0,     3,
       0,     0,     0,     0,     0,     0,     9,    51,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,     0,     0,     0,     0,     0,    12,    10,     0,    43,
      44,   115,    29,    66,    67,    64,    65,     0,     0,     0,
       0,     0,    17,     0,    16,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       5,     0,     0,     0,     0,     0,     0,     0,     6,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     8,
       0,     0,     0,     0,     0,     0,     0,     7,     0,    55,
       0,    52,    54,     0,    53,     0,    96,    97,    98,     0,
       0,   104,     0,   107,     0,     0,     0,     0,    45,   116,
      30,   121,    15,    11,    13,    14,     0,    81,    83,    82,
      79,    80,    78,    77,    38,    39,    37,    40,    46,    41,
      42,     0,     0,     0,     0,    91,    90,    93,    92,    47,
       0,     0,     0,    72,    71,    76,    74,    73,    75,    23,
      22,    21,     0,    85,    84,    87,    89,    88,    86,   122,
       0,     0,     0,     0,     0,     0,     0,     0,    34,    70,
     119,    20,     0,     0,    59,     0,     0,     0,     0,   110,
      29,     0,     0,    24,     0,    57,    99,     0,   124,     0,
      56,     0,   105,     0,    94,     0,    48,    50,    49,    95,
     123,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,   111,     0,    25,     0,   125,     0,   100,     0,     0,
     108,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,   112,     0,    26,    58,     0,   106,   109,     0,     0,
       0,     0,     0,    62,     0,   113,     0,    27,     0,   101,
       0,     0,     0,     0,    63,   114,    28,     0,     0,   102
};

/* FFDEFGOTO[NTERM-NUM].  */
static const fftype_int8 ffdefgoto[] =
{
      -1,     1,    29,    30,    31,    46,    47,    44,    58
};

/* FFPACT[STATE-NUM] -- Index in FFTABLE of the portion describing
   STATE-NUM.  */
#define FFPACT_NINF -46
static const fftype_int16 ffpact[] =
{
     -46,   297,   -46,   -45,   -46,   -46,   -46,   -46,   -46,   347,
     398,   398,    -5,     0,    -4,     5,    19,    23,   -46,   -46,
     -46,   398,   398,   398,   398,   398,   398,   -46,   398,   -46,
       6,    17,  1088,   296,  1468,  1490,   -46,   -46,   424,     9,
    1374,   135,   452,   168,  1518,   344,  1355,  1449,  1555,   -10,
     -46,    -9,   398,   398,   398,   398,  1355,  1449,   250,    -2,
      -2,    10,    11,    -2,    10,    -2,    10,   619,   240,  1399,
    1424,   398,   -46,   398,   -46,   398,   398,   398,   398,   398,
     398,   398,   398,   398,   398,   398,   398,   398,   398,   398,
     -46,   398,   398,   398,   398,   398,   398,   398,   -46,    -3,
      -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,   398,   -46,
     398,   398,   398,   398,   398,   398,   398,   -46,   398,   -46,
     398,   -46,   -46,   398,   -46,   398,   -46,   -46,   -46,   398,
     398,   -46,   398,   -46,  1231,  1251,  1271,  1291,   -46,   -46,
     -46,   -46,  1355,  1449,  1355,  1449,  1313,  1535,  1535,  1535,
    1556,  1556,  1556,  1556,    55,    55,    55,   -40,    10,   -40,
     -40,   728,  1335,   400,   201,    74,   111,   -35,   -35,   -40,
     752,    -3,    -3,    24,    24,    24,    24,    24,    24,    79,
      11,    11,   776,   -17,   -17,    28,    28,    28,    28,   -46,
     480,   342,  1111,  1431,  1131,  1438,   508,  1151,   -46,   -46,
     -46,   -46,   398,   398,   -46,   398,   398,   398,   398,   -46,
      11,    20,   398,   -46,   398,   -46,   -46,   398,   -46,   398,
     -46,    60,   -46,   398,  1499,   800,  1499,  1449,  1499,  1449,
     250,   824,   848,  1171,   646,   536,    48,   564,   398,   -46,
     398,   -46,   398,   -46,   398,   -46,   398,   -46,    63,    83,
     -46,   872,   896,   920,   673,  1191,    39,    45,   398,   -46,
     398,   -46,   398,   -46,   -46,   398,   -46,   -46,   944,   968,
     992,   592,   398,   -46,   398,   -46,   398,   -46,   398,   -46,
    1016,  1040,  1064,  1211,   -46,   -46,   -46,   398,   700,   -46
};

/* FFPGOTO[NTERM-NUM].  */
static const fftype_int16 ffpgoto[] =
{
     -46,   -46,   -46,   -46,   -46,    -1,    90,   147,    22
};

/* FFTABLE[FFPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what FFDEFACT says.
   If FFTABLE_NINF, syntax error.  */
#define FFTABLE_NINF -1
static const fftype_uint16 fftable[] =
{
      32,    49,    88,    96,     8,    36,    51,    89,    38,    42,
     130,   132,    97,    17,   112,   113,   114,   115,   116,    52,
      56,    59,    60,    35,    63,    65,    71,    67,    53,   120,
      72,    41,    45,    48,    91,    92,    93,    73,    94,    95,
     171,    74,    54,   131,   133,    89,    55,    96,    50,   172,
      70,   134,   135,   136,   137,   105,    97,    97,   108,   105,
     106,   107,   121,   116,   106,   107,   236,   108,   248,   256,
     142,   108,   144,   140,   146,   147,   148,   149,   150,   151,
     152,   153,   154,   155,   156,   157,   159,   160,   161,   257,
     162,    33,   266,    86,    87,   169,   170,    88,   267,    39,
      43,    93,    89,    94,    95,     0,     0,   182,     0,     0,
       0,    57,    96,   164,    61,    64,    66,   190,    68,   106,
     107,    97,   192,     0,   194,     0,   108,     0,     0,   196,
       0,   197,   183,   184,   185,   186,   187,   188,   189,     0,
      94,    95,     0,     0,     0,   193,     0,     0,    34,    96,
       0,   195,     0,     0,     0,   123,    40,     0,    97,     0,
       0,   143,     0,   145,   110,   111,   112,   113,   114,   115,
     116,    62,     0,     0,     0,    69,   158,     0,     0,     0,
       0,   163,   165,   166,   167,   168,     0,     0,   124,     0,
       0,     0,     0,    91,    92,    93,     0,    94,    95,     0,
       0,   224,   225,     0,   226,   228,    96,   231,     0,     0,
     191,   232,     0,   233,     0,    97,   234,     0,   235,     0,
       0,   127,   237,   207,     0,     0,     0,     0,     0,   230,
     110,   111,   112,   113,   114,   115,   116,   251,     0,   252,
       0,   253,     0,   254,     0,   255,   173,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   268,     0,   269,
       0,   270,     0,     0,   271,    91,    92,    93,     0,    94,
      95,   280,     0,   281,     0,   282,     0,   283,    96,   110,
     111,   112,   113,   114,   115,   116,   288,    97,     0,     0,
       0,     0,     0,   139,     0,   227,   229,     2,     3,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,     0,   210,   211,
      21,    91,    92,    93,     0,    94,    95,     0,     0,     0,
       0,     0,    22,    23,    96,     0,     0,     0,     0,     0,
      24,    25,    26,    97,     0,     0,    98,    27,     0,    28,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    91,    92,    93,
      21,    94,    95,   110,   111,   112,   113,   114,   115,   116,
      96,     0,    22,    23,     0,     0,     0,     0,     0,    97,
      24,    25,    26,     0,     0,   216,     0,   128,     0,    28,
      37,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,     0,     0,
       0,    21,   206,     0,     0,    91,    92,    93,     0,    94,
      95,     0,     0,    22,    23,     0,     0,     0,    96,     0,
       0,    24,    25,    26,   118,    75,     0,    97,     0,     0,
      28,     0,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,     0,     0,    88,     0,     0,     0,
       0,    89,   125,    75,     0,     0,     0,   119,     0,     0,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,     0,     0,    88,     0,     0,     0,     0,    89,
     214,    75,     0,     0,     0,   126,     0,     0,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
       0,     0,    88,     0,     0,     0,     0,    89,   221,    75,
       0,     0,     0,   215,     0,     0,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,     0,     0,
      88,     0,     0,     0,     0,    89,   246,    75,     0,     0,
       0,   222,     0,     0,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,     0,     0,    88,     0,
       0,     0,     0,    89,   249,    75,     0,     0,     0,   247,
       0,     0,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,     0,     0,    88,     0,     0,     0,
       0,    89,   278,    75,     0,     0,     0,   250,     0,     0,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,     0,     0,    88,     0,     0,     0,     0,    89,
      75,     0,     0,     0,     0,   279,     0,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,     0,
       0,    88,     0,     0,     0,     0,    89,    75,     0,     0,
       0,     0,   138,     0,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,     0,     0,    88,     0,
       0,     0,     0,    89,    75,     0,     0,     0,     0,   245,
       0,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,     0,     0,    88,     0,     0,     0,     0,
      89,    75,     0,     0,     0,     0,   264,     0,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
       0,     0,    88,     0,     0,     0,     0,    89,   203,    75,
       0,     0,     0,   289,     0,     0,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,     0,     0,
      88,     0,   208,    75,     0,    89,     0,     0,     0,   204,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,     0,     0,    88,     0,   212,    75,     0,    89,
       0,     0,     0,   209,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,     0,     0,    88,     0,
     238,    75,     0,    89,     0,     0,     0,   213,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
       0,     0,    88,     0,   240,    75,     0,    89,     0,     0,
       0,   239,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,     0,     0,    88,     0,   242,    75,
       0,    89,     0,     0,     0,   241,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,     0,     0,
      88,     0,   258,    75,     0,    89,     0,     0,     0,   243,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,     0,     0,    88,     0,   260,    75,     0,    89,
       0,     0,     0,   259,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,     0,     0,    88,     0,
     262,    75,     0,    89,     0,     0,     0,   261,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
       0,     0,    88,     0,   272,    75,     0,    89,     0,     0,
       0,   263,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,     0,     0,    88,     0,   274,    75,
       0,    89,     0,     0,     0,   273,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,     0,     0,
      88,     0,   276,    75,     0,    89,     0,     0,     0,   275,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,     0,     0,    88,     0,     0,    75,     0,    89,
       0,     0,     0,   277,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,     0,     0,    88,     0,
       0,    75,     0,    89,     0,     0,     0,   284,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
       0,     0,    88,     0,     0,    75,     0,    89,     0,     0,
       0,   285,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,     0,     0,    88,     0,     0,    75,
       0,    89,     0,     0,     0,   286,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,     0,     0,
      88,   217,    75,     0,     0,    89,     0,     0,    90,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,   219,    75,    88,     0,     0,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,   223,    75,    88,     0,     0,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,   244,    75,    88,     0,     0,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,   265,    75,    88,     0,     0,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,   287,    75,    88,     0,     0,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,     0,    75,    88,     0,   198,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,     0,    75,    88,     0,   199,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,     0,    75,    88,     0,   200,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,     0,    75,    88,     0,   201,     0,     0,    89,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,     0,     0,    88,    75,   202,     0,     0,    89,     0,
       0,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,     0,     0,    88,    75,   205,     0,     0,
      89,     0,     0,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,     0,    75,    88,     0,     0,
       0,     0,    89,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,     0,     0,    88,     0,     0,
       0,     0,    89,    99,   100,   101,   102,   103,   104,   105,
       0,     0,     0,     0,   106,   107,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   122,    99,   100,
     101,   102,   103,   104,   105,     0,     0,     0,     0,   106,
     107,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   140,   110,   111,   112,   113,   114,   115,   116,
     110,   111,   112,   113,   114,   115,   116,   110,   111,   112,
     113,   114,   115,   116,    91,    92,    93,   141,    94,    95,
       0,     0,     0,     0,   218,     0,     0,    96,     0,     0,
       0,   220,     0,     0,     0,     0,    97,    99,   100,   101,
     102,   103,   104,   105,     0,     0,     0,     0,   106,   107,
       0,     0,     0,     0,     0,   108,     0,     0,   109,   110,
     111,   112,   113,   114,   115,   116,     0,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,     0,
     117,    88,     0,     0,     0,     0,    89,    99,   100,   101,
     102,   103,   104,   105,     0,     0,     0,     0,   106,   107,
       0,     0,     0,     0,     0,   108,    79,    80,    81,    82,
      83,    84,    85,    86,    87,   129,     0,    88,     0,     0,
       0,     0,    89,     0,   110,   111,   112,   113,   114,   115,
     116,    83,    84,    85,    86,    87,     0,     0,    88,     0,
       0,     0,     0,    89
};

static const fftype_int16 ffcheck[] =
{
       1,     6,    42,    38,     7,    50,     6,    47,     9,    10,
      20,    20,    47,    16,    31,    32,    33,    34,    35,    23,
      21,    22,    23,     1,    25,    26,    20,    28,    23,    20,
      24,     9,    10,    11,    25,    26,    27,    20,    29,    30,
      43,    24,    23,    53,    53,    47,    23,    38,    53,    52,
      28,    52,    53,    54,    55,    35,    47,    47,    47,    35,
      40,    41,    53,    35,    40,    41,     6,    47,    20,     6,
      71,    47,    73,    53,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,     6,
      91,     1,    53,    38,    39,    96,    97,    42,    53,     9,
      10,    27,    47,    29,    30,    -1,    -1,   108,    -1,    -1,
      -1,    21,    38,    91,    24,    25,    26,   118,    28,    40,
      41,    47,   123,    -1,   125,    -1,    47,    -1,    -1,   130,
      -1,   132,   110,   111,   112,   113,   114,   115,   116,    -1,
      29,    30,    -1,    -1,    -1,   123,    -1,    -1,     1,    38,
      -1,   129,    -1,    -1,    -1,    20,     9,    -1,    47,    -1,
      -1,    71,    -1,    73,    29,    30,    31,    32,    33,    34,
      35,    24,    -1,    -1,    -1,    28,    86,    -1,    -1,    -1,
      -1,    91,    92,    93,    94,    95,    -1,    -1,    53,    -1,
      -1,    -1,    -1,    25,    26,    27,    -1,    29,    30,    -1,
      -1,   202,   203,    -1,   205,   206,    38,   208,    -1,    -1,
     120,   212,    -1,   214,    -1,    47,   217,    -1,   219,    -1,
      -1,    53,   223,    22,    -1,    -1,    -1,    -1,    -1,   207,
      29,    30,    31,    32,    33,    34,    35,   238,    -1,   240,
      -1,   242,    -1,   244,    -1,   246,    99,   100,   101,   102,
     103,   104,   105,   106,   107,    -1,    -1,   258,    -1,   260,
      -1,   262,    -1,    -1,   265,    25,    26,    27,    -1,    29,
      30,   272,    -1,   274,    -1,   276,    -1,   278,    38,    29,
      30,    31,    32,    33,    34,    35,   287,    47,    -1,    -1,
      -1,    -1,    -1,    53,    -1,   205,   206,     0,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    -1,   171,   172,
      23,    25,    26,    27,    -1,    29,    30,    -1,    -1,    -1,
      -1,    -1,    35,    36,    38,    -1,    -1,    -1,    -1,    -1,
      43,    44,    45,    47,    -1,    -1,    50,    50,    -1,    52,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    25,    26,    27,
      23,    29,    30,    29,    30,    31,    32,    33,    34,    35,
      38,    -1,    35,    36,    -1,    -1,    -1,    -1,    -1,    47,
      43,    44,    45,    -1,    -1,    53,    -1,    53,    -1,    52,
      53,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    -1,    -1,
      -1,    23,    22,    -1,    -1,    25,    26,    27,    -1,    29,
      30,    -1,    -1,    35,    36,    -1,    -1,    -1,    38,    -1,
      -1,    43,    44,    45,    20,    21,    -1,    47,    -1,    -1,
      52,    -1,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,
      -1,    47,    20,    21,    -1,    -1,    -1,    53,    -1,    -1,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,    47,
      20,    21,    -1,    -1,    -1,    53,    -1,    -1,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    -1,    -1,    -1,    47,    20,    21,
      -1,    -1,    -1,    53,    -1,    -1,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      42,    -1,    -1,    -1,    -1,    47,    20,    21,    -1,    -1,
      -1,    53,    -1,    -1,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    42,    -1,
      -1,    -1,    -1,    47,    20,    21,    -1,    -1,    -1,    53,
      -1,    -1,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,
      -1,    47,    20,    21,    -1,    -1,    -1,    53,    -1,    -1,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,    47,
      21,    -1,    -1,    -1,    -1,    53,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    42,    -1,    -1,    -1,    -1,    47,    21,    -1,    -1,
      -1,    -1,    53,    -1,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    42,    -1,
      -1,    -1,    -1,    47,    21,    -1,    -1,    -1,    -1,    53,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,
      47,    21,    -1,    -1,    -1,    -1,    53,    -1,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    -1,    -1,    -1,    47,    20,    21,
      -1,    -1,    -1,    53,    -1,    -1,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      42,    -1,    20,    21,    -1,    47,    -1,    -1,    -1,    51,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    42,    -1,    20,    21,    -1,    47,
      -1,    -1,    -1,    51,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    42,    -1,
      20,    21,    -1,    47,    -1,    -1,    -1,    51,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    20,    21,    -1,    47,    -1,    -1,
      -1,    51,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    42,    -1,    20,    21,
      -1,    47,    -1,    -1,    -1,    51,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      42,    -1,    20,    21,    -1,    47,    -1,    -1,    -1,    51,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    42,    -1,    20,    21,    -1,    47,
      -1,    -1,    -1,    51,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    42,    -1,
      20,    21,    -1,    47,    -1,    -1,    -1,    51,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    20,    21,    -1,    47,    -1,    -1,
      -1,    51,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    42,    -1,    20,    21,
      -1,    47,    -1,    -1,    -1,    51,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      42,    -1,    20,    21,    -1,    47,    -1,    -1,    -1,    51,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    42,    -1,    -1,    21,    -1,    47,
      -1,    -1,    -1,    51,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    42,    -1,
      -1,    21,    -1,    47,    -1,    -1,    -1,    51,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    42,    -1,    -1,    21,    -1,    47,    -1,    -1,
      -1,    51,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,    21,
      -1,    47,    -1,    -1,    -1,    51,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      42,    20,    21,    -1,    -1,    47,    -1,    -1,    50,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    20,    21,    42,    -1,    -1,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    20,    21,    42,    -1,    -1,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    20,    21,    42,    -1,    -1,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    20,    21,    42,    -1,    -1,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    20,    21,    42,    -1,    -1,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    21,    42,    -1,    24,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    21,    42,    -1,    24,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    21,    42,    -1,    24,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    21,    42,    -1,    24,    -1,    -1,    47,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    42,    21,    22,    -1,    -1,    47,    -1,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    42,    21,    22,    -1,    -1,
      47,    -1,    -1,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    21,    42,    -1,    -1,
      -1,    -1,    47,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    42,    -1,    -1,
      -1,    -1,    47,    29,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    -1,    40,    41,    -1,    -1,    -1,    -1,
      -1,    47,    -1,    -1,    -1,    -1,    -1,    53,    29,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    -1,    40,
      41,    -1,    -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,
      -1,    -1,    53,    29,    30,    31,    32,    33,    34,    35,
      29,    30,    31,    32,    33,    34,    35,    29,    30,    31,
      32,    33,    34,    35,    25,    26,    27,    53,    29,    30,
      -1,    -1,    -1,    -1,    53,    -1,    -1,    38,    -1,    -1,
      -1,    53,    -1,    -1,    -1,    -1,    47,    29,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    47,    -1,    -1,    50,    29,
      30,    31,    32,    33,    34,    35,    -1,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      50,    42,    -1,    -1,    -1,    -1,    47,    29,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    47,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    20,    -1,    42,    -1,    -1,
      -1,    -1,    47,    -1,    29,    30,    31,    32,    33,    34,
      35,    35,    36,    37,    38,    39,    -1,    -1,    42,    -1,
      -1,    -1,    -1,    47
};

/* FFSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const fftype_uint8 ffstos[] =
{
       0,    55,     0,     1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    23,    35,    36,    43,    44,    45,    50,    52,    56,
      57,    58,    59,    60,    61,    62,    50,    53,    59,    60,
      61,    62,    59,    60,    61,    62,    59,    60,    62,     6,
      53,     6,    23,    23,    23,    23,    59,    60,    62,    59,
      59,    60,    61,    59,    60,    59,    60,    59,    60,    61,
      62,    20,    24,    20,    24,    21,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    42,    47,
      50,    25,    26,    27,    29,    30,    38,    47,    50,    29,
      30,    31,    32,    33,    34,    35,    40,    41,    47,    50,
      29,    30,    31,    32,    33,    34,    35,    50,    20,    53,
      20,    53,    53,    20,    53,    20,    53,    53,    53,    20,
      20,    53,    20,    53,    59,    59,    59,    59,    53,    53,
      53,    53,    59,    60,    59,    60,    59,    59,    59,    59,
      59,    59,    59,    59,    59,    59,    59,    59,    60,    59,
      59,    59,    59,    60,    62,    60,    60,    60,    60,    59,
      59,    43,    52,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    59,    62,    62,    62,    62,    62,    62,    62,
      59,    60,    59,    62,    59,    62,    59,    59,    24,    24,
      24,    24,    22,    20,    51,    22,    22,    22,    20,    51,
      61,    61,    20,    51,    20,    53,    53,    20,    53,    20,
      53,    20,    53,    20,    59,    59,    59,    60,    59,    60,
      62,    59,    59,    59,    59,    59,     6,    59,    20,    51,
      20,    51,    20,    51,    20,    53,    20,    53,    20,    20,
      53,    59,    59,    59,    59,    59,     6,     6,    20,    51,
      20,    51,    20,    51,    53,    20,    53,    53,    59,    59,
      59,    59,    20,    51,    20,    51,    20,    51,    20,    53,
      59,    59,    59,    59,    51,    51,    51,    20,    59,    53
};

#define fferrok		(fferrstatus = 0)
#define ffclearin	(ffchar = FFEMPTY)
#define FFEMPTY		(-2)
#define FFEOF		0

#define FFACCEPT	goto ffacceptlab
#define FFABORT		goto ffabortlab
#define FFERROR		goto fferrorlab


/* Like FFERROR except do call fferror.  This remains here temporarily
   to ease the transition to the new meaning of FFERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define FFFAIL		goto fferrlab

#define FFRECOVERING()  (!!fferrstatus)

#define FFBACKUP(Token, Value)					\
do								\
  if (ffchar == FFEMPTY && fflen == 1)				\
    {								\
      ffchar = (Token);						\
      fflval = (Value);						\
      fftoken = FFTRANSLATE (ffchar);				\
      FFPOPSTACK (1);						\
      goto ffbackup;						\
    }								\
  else								\
    {								\
      fferror (FF_("syntax error: cannot back up")); \
      FFERROR;							\
    }								\
while (FFID (0))

>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0

#define FFBACKUP(Token, Value)                                    \
  do                                                              \
    if (ffchar == FFEMPTY)                                        \
      {                                                           \
        ffchar = (Token);                                         \
        fflval = (Value);                                         \
        FFPOPSTACK (fflen);                                       \
        ffstate = *ffssp;                                         \
        goto ffbackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        fferror (FF_("syntax error: cannot back up")); \
        FFERROR;                                                  \
      }                                                           \
  while (0)

<<<<<<< HEAD
/* Backward compatibility with an undocumented macro.
   Use FFerror or FFUNDEF. */
#define FFERRCODE FFUNDEF


/* Enable debugging if requested.  */
#if FFDEBUG

# ifndef FFFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define FFFPRINTF fprintf
# endif

# define FFDPRINTF(Args)                        \
do {                                            \
  if (ffdebug)                                  \
    FFFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
# ifndef FF_LOCATION_PRINT
#  define FF_LOCATION_PRINT(File, Loc) ((void) 0)
# endif


# define FF_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (ffdebug)                                                            \
    {                                                                     \
      FFFPRINTF (stderr, "%s ", Title);                                   \
      ff_symbol_print (stderr,                                            \
                  Kind, Value); \
      FFFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on FFO.  |
`-----------------------------------*/

static void
ff_symbol_value_print (FILE *ffo,
                       ffsymbol_kind_t ffkind, FFSTYPE const * const ffvaluep)
{
  FILE *ffoutput = ffo;
  FFUSE (ffoutput);
  if (!ffvaluep)
    return;
# ifdef FFPRINT
  if (ffkind < FFNTOKENS)
    FFPRINT (ffo, fftoknum[ffkind], *ffvaluep);
# endif
  FF_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  FFUSE (ffkind);
  FF_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on FFO.  |
`---------------------------*/

static void
ff_symbol_print (FILE *ffo,
                 ffsymbol_kind_t ffkind, FFSTYPE const * const ffvaluep)
{
  FFFPRINTF (ffo, "%s %s (",
             ffkind < FFNTOKENS ? "token" : "nterm", ffsymbol_name (ffkind));

  ff_symbol_value_print (ffo, ffkind, ffvaluep);
  FFFPRINTF (ffo, ")");
}

/*------------------------------------------------------------------.
| ff_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
ff_stack_print (ff_state_t *ffbottom, ff_state_t *fftop)
{
  FFFPRINTF (stderr, "Stack now");
  for (; ffbottom <= fftop; ffbottom++)
    {
      int ffbot = *ffbottom;
      FFFPRINTF (stderr, " %d", ffbot);
    }
  FFFPRINTF (stderr, "\n");
}

# define FF_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (ffdebug)                                                  \
    ff_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the FFRULE is going to be reduced.  |
`------------------------------------------------*/

static void
ff_reduce_print (ff_state_t *ffssp, FFSTYPE *ffvsp,
                 int ffrule)
{
  int fflno = ffrline[ffrule];
  int ffnrhs = ffr2[ffrule];
  int ffi;
  FFFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             ffrule - 1, fflno);
  /* The symbols being reduced.  */
  for (ffi = 0; ffi < ffnrhs; ffi++)
    {
      FFFPRINTF (stderr, "   $%d = ", ffi + 1);
      ff_symbol_print (stderr,
                       FF_ACCESSING_SYMBOL (+ffssp[ffi + 1 - ffnrhs]),
                       &ffvsp[(ffi + 1) - (ffnrhs)]);
      FFFPRINTF (stderr, "\n");
    }
}

# define FF_REDUCE_PRINT(Rule)          \
do {                                    \
  if (ffdebug)                          \
    ff_reduce_print (ffssp, ffvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int ffdebug;
#else /* !FFDEBUG */
# define FFDPRINTF(Args) ((void) 0)
# define FF_SYMBOL_PRINT(Title, Kind, Value, Location)
# define FF_STACK_PRINT(Bottom, Top)
# define FF_REDUCE_PRINT(Rule)
#endif /* !FFDEBUG */


/* FFINITDEPTH -- initial size of the parser's stacks.  */
#ifndef FFINITDEPTH
# define FFINITDEPTH 200
#endif

/* FFMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

=======

/* FFLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define FFRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef FFLLOC_DEFAULT
# define FFLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (FFID (N))                                                    \
	{								\
	  (Current).first_line   = FFRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = FFRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = FFRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = FFRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    FFRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    FFRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (FFID (0))
#endif


/* FF_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef FF_LOCATION_PRINT
# if FFLTYPE_IS_TRIVIAL
#  define FF_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define FF_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* FFLEX -- calling `fflex' with the right arguments.  */

#ifdef FFLEX_PARAM
# define FFLEX fflex (FFLEX_PARAM)
#else
# define FFLEX fflex ()
#endif

/* Enable debugging if requested.  */
#if FFDEBUG

# ifndef FFFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define FFFPRINTF fprintf
# endif

# define FFDPRINTF(Args)			\
do {						\
  if (ffdebug)					\
    FFFPRINTF Args;				\
} while (FFID (0))

# define FF_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (ffdebug)								  \
    {									  \
      FFFPRINTF (stderr, "%s ", Title);					  \
      ff_symbol_print (stderr,						  \
		  Type, Value); \
      FFFPRINTF (stderr, "\n");						  \
    }									  \
} while (FFID (0))


/*--------------------------------.
| Print this symbol on FFOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
ff_symbol_value_print (FILE *ffoutput, int fftype, FFSTYPE const * const ffvaluep)
#else
static void
ff_symbol_value_print (ffoutput, fftype, ffvaluep)
    FILE *ffoutput;
    int fftype;
    FFSTYPE const * const ffvaluep;
#endif
{
  if (!ffvaluep)
    return;
# ifdef FFPRINT
  if (fftype < FFNTOKENS)
    FFPRINT (ffoutput, fftoknum[fftype], *ffvaluep);
# else
  FFUSE (ffoutput);
# endif
  switch (fftype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on FFOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
ff_symbol_print (FILE *ffoutput, int fftype, FFSTYPE const * const ffvaluep)
#else
static void
ff_symbol_print (ffoutput, fftype, ffvaluep)
    FILE *ffoutput;
    int fftype;
    FFSTYPE const * const ffvaluep;
#endif
{
  if (fftype < FFNTOKENS)
    FFFPRINTF (ffoutput, "token %s (", fftname[fftype]);
  else
    FFFPRINTF (ffoutput, "nterm %s (", fftname[fftype]);

  ff_symbol_value_print (ffoutput, fftype, ffvaluep);
  FFFPRINTF (ffoutput, ")");
}

/*------------------------------------------------------------------.
| ff_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
ff_stack_print (fftype_int16 *ffbottom, fftype_int16 *fftop)
#else
static void
ff_stack_print (ffbottom, fftop)
    fftype_int16 *ffbottom;
    fftype_int16 *fftop;
#endif
{
  FFFPRINTF (stderr, "Stack now");
  for (; ffbottom <= fftop; ffbottom++)
    {
      int ffbot = *ffbottom;
      FFFPRINTF (stderr, " %d", ffbot);
    }
  FFFPRINTF (stderr, "\n");
}

# define FF_STACK_PRINT(Bottom, Top)				\
do {								\
  if (ffdebug)							\
    ff_stack_print ((Bottom), (Top));				\
} while (FFID (0))


/*------------------------------------------------.
| Report that the FFRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
ff_reduce_print (FFSTYPE *ffvsp, int ffrule)
#else
static void
ff_reduce_print (ffvsp, ffrule)
    FFSTYPE *ffvsp;
    int ffrule;
#endif
{
  int ffnrhs = ffr2[ffrule];
  int ffi;
  unsigned long int fflno = ffrline[ffrule];
  FFFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     ffrule - 1, fflno);
  /* The symbols being reduced.  */
  for (ffi = 0; ffi < ffnrhs; ffi++)
    {
      FFFPRINTF (stderr, "   $%d = ", ffi + 1);
      ff_symbol_print (stderr, ffrhs[ffprhs[ffrule] + ffi],
		       &(ffvsp[(ffi + 1) - (ffnrhs)])
		       		       );
      FFFPRINTF (stderr, "\n");
    }
}

# define FF_REDUCE_PRINT(Rule)		\
do {					\
  if (ffdebug)				\
    ff_reduce_print (ffvsp, Rule); \
} while (FFID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int ffdebug;
#else /* !FFDEBUG */
# define FFDPRINTF(Args)
# define FF_SYMBOL_PRINT(Title, Type, Value, Location)
# define FF_STACK_PRINT(Bottom, Top)
# define FF_REDUCE_PRINT(Rule)
#endif /* !FFDEBUG */


/* FFINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	FFINITDEPTH
# define FFINITDEPTH 200
#endif

/* FFMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
   Do not make this value too large; the results are undefined if
   FFSTACK_ALLOC_MAXIMUM < FFSTACK_BYTES (FFMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef FFMAXDEPTH
# define FFMAXDEPTH 10000
#endif

<<<<<<< HEAD





/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
ffdestruct (const char *ffmsg,
            ffsymbol_kind_t ffkind, FFSTYPE *ffvaluep)
{
  FFUSE (ffvaluep);
  if (!ffmsg)
    ffmsg = "Deleting";
  FF_SYMBOL_PRINT (ffmsg, ffkind, ffvaluep, fflocationp);

  FF_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  FFUSE (ffkind);
  FF_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int ffchar;

/* The semantic value of the lookahead symbol.  */
FFSTYPE fflval;
/* Number of syntax errors so far.  */
int ffnerrs;




/*----------.
| ffparse.  |
`----------*/

int
ffparse (void)
{
    ff_state_fast_t ffstate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int fferrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow ffoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    FFPTRDIFF_T ffstacksize = FFINITDEPTH;

    /* The state stack: array, bottom, top.  */
    ff_state_t ffssa[FFINITDEPTH];
    ff_state_t *ffss = ffssa;
    ff_state_t *ffssp = ffss;

    /* The semantic value stack: array, bottom, top.  */
    FFSTYPE ffvsa[FFINITDEPTH];
    FFSTYPE *ffvs = ffvsa;
    FFSTYPE *ffvsp = ffvs;

  int ffn;
  /* The return value of ffparse.  */
  int ffresult;
  /* Lookahead symbol kind.  */
  ffsymbol_kind_t fftoken = FFSYMBOL_FFEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  FFSTYPE ffval;



#define FFPOPSTACK(N)   (ffvsp -= (N), ffssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int fflen = 0;

  FFDPRINTF ((stderr, "Starting parse\n"));

  ffchar = FFEMPTY; /* Cause a token to be read.  */
  goto ffsetstate;


/*------------------------------------------------------------.
| ffnewstate -- push a new state, which is found in ffstate.  |
`------------------------------------------------------------*/
ffnewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  ffssp++;


/*--------------------------------------------------------------------.
| ffsetstate -- set current state (the top of the stack) to ffstate.  |
`--------------------------------------------------------------------*/
ffsetstate:
  FFDPRINTF ((stderr, "Entering state %d\n", ffstate));
  FF_ASSERT (0 <= ffstate && ffstate < FFNSTATES);
  FF_IGNORE_USELESS_CAST_BEGIN
  *ffssp = FF_CAST (ff_state_t, ffstate);
  FF_IGNORE_USELESS_CAST_END
  FF_STACK_PRINT (ffss, ffssp);

  if (ffss + ffstacksize - 1 <= ffssp)
#if !defined ffoverflow && !defined FFSTACK_RELOCATE
    goto ffexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      FFPTRDIFF_T ffsize = ffssp - ffss + 1;

# if defined ffoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        ff_state_t *ffss1 = ffss;
        FFSTYPE *ffvs1 = ffvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if ffoverflow is a macro.  */
        ffoverflow (FF_("memory exhausted"),
                    &ffss1, ffsize * FFSIZEOF (*ffssp),
                    &ffvs1, ffsize * FFSIZEOF (*ffvsp),
                    &ffstacksize);
        ffss = ffss1;
        ffvs = ffvs1;
      }
# else /* defined FFSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (FFMAXDEPTH <= ffstacksize)
        goto ffexhaustedlab;
      ffstacksize *= 2;
      if (FFMAXDEPTH < ffstacksize)
        ffstacksize = FFMAXDEPTH;

      {
        ff_state_t *ffss1 = ffss;
        union ffalloc *ffptr =
          FF_CAST (union ffalloc *,
                   FFSTACK_ALLOC (FF_CAST (FFSIZE_T, FFSTACK_BYTES (ffstacksize))));
        if (! ffptr)
          goto ffexhaustedlab;
        FFSTACK_RELOCATE (ffss_alloc, ffss);
        FFSTACK_RELOCATE (ffvs_alloc, ffvs);
#  undef FFSTACK_RELOCATE
        if (ffss1 != ffssa)
          FFSTACK_FREE (ffss1);
      }
# endif

      ffssp = ffss + ffsize - 1;
      ffvsp = ffvs + ffsize - 1;

      FF_IGNORE_USELESS_CAST_BEGIN
      FFDPRINTF ((stderr, "Stack size increased to %ld\n",
                  FF_CAST (long, ffstacksize)));
      FF_IGNORE_USELESS_CAST_END

      if (ffss + ffstacksize - 1 <= ffssp)
        FFABORT;
=======


#if FFERROR_VERBOSE

# ifndef ffstrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define ffstrlen strlen
#  else
/* Return the length of FFSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static FFSIZE_T
ffstrlen (const char *ffstr)
#else
static FFSIZE_T
ffstrlen (ffstr)
    const char *ffstr;
#endif
{
  FFSIZE_T fflen;
  for (fflen = 0; ffstr[fflen]; fflen++)
    continue;
  return fflen;
}
#  endif
# endif

# ifndef ffstpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define ffstpcpy stpcpy
#  else
/* Copy FFSRC to FFDEST, returning the address of the terminating '\0' in
   FFDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
ffstpcpy (char *ffdest, const char *ffsrc)
#else
static char *
ffstpcpy (ffdest, ffsrc)
    char *ffdest;
    const char *ffsrc;
#endif
{
  char *ffd = ffdest;
  const char *ffs = ffsrc;

  while ((*ffd++ = *ffs++) != '\0')
    continue;

  return ffd - 1;
}
#  endif
# endif

# ifndef fftnamerr
/* Copy to FFRES the contents of FFSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for fferror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  FFSTR is taken from fftname.  If FFRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static FFSIZE_T
fftnamerr (char *ffres, const char *ffstr)
{
  if (*ffstr == '"')
    {
      FFSIZE_T ffn = 0;
      char const *ffp = ffstr;

      for (;;)
	switch (*++ffp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++ffp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (ffres)
	      ffres[ffn] = *ffp;
	    ffn++;
	    break;

	  case '"':
	    if (ffres)
	      ffres[ffn] = '\0';
	    return ffn;
	  }
    do_not_strip_quotes: ;
    }

  if (! ffres)
    return ffstrlen (ffstr);

  return ffstpcpy (ffres, ffstr) - ffres;
}
# endif

/* Copy into FFRESULT an error message about the unexpected token
   FFCHAR while in state FFSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If FFRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return FFSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static FFSIZE_T
ffsyntax_error (char *ffresult, int ffstate, int ffchar)
{
  int ffn = ffpact[ffstate];

  if (! (FFPACT_NINF < ffn && ffn <= FFLAST))
    return 0;
  else
    {
      int fftype = FFTRANSLATE (ffchar);
      FFSIZE_T ffsize0 = fftnamerr (0, fftname[fftype]);
      FFSIZE_T ffsize = ffsize0;
      FFSIZE_T ffsize1;
      int ffsize_overflow = 0;
      enum { FFERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *ffarg[FFERROR_VERBOSE_ARGS_MAXIMUM];
      int ffx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      FF_("syntax error, unexpected %s");
      FF_("syntax error, unexpected %s, expecting %s");
      FF_("syntax error, unexpected %s, expecting %s or %s");
      FF_("syntax error, unexpected %s, expecting %s or %s or %s");
      FF_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *fffmt;
      char const *fff;
      static char const ffunexpected[] = "syntax error, unexpected %s";
      static char const ffexpecting[] = ", expecting %s";
      static char const ffor[] = " or %s";
      char ffformat[sizeof ffunexpected
		    + sizeof ffexpecting - 1
		    + ((FFERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof ffor - 1))];
      char const *ffprefix = ffexpecting;

      /* Start FFX at -FFN if negative to avoid negative indexes in
	 FFCHECK.  */
      int ffxbegin = ffn < 0 ? -ffn : 0;

      /* Stay within bounds of both ffcheck and fftname.  */
      int ffchecklim = FFLAST - ffn + 1;
      int ffxend = ffchecklim < FFNTOKENS ? ffchecklim : FFNTOKENS;
      int ffcount = 1;

      ffarg[0] = fftname[fftype];
      fffmt = ffstpcpy (ffformat, ffunexpected);

      for (ffx = ffxbegin; ffx < ffxend; ++ffx)
	if (ffcheck[ffx + ffn] == ffx && ffx != FFTERROR)
	  {
	    if (ffcount == FFERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		ffcount = 1;
		ffsize = ffsize0;
		ffformat[sizeof ffunexpected - 1] = '\0';
		break;
	      }
	    ffarg[ffcount++] = fftname[ffx];
	    ffsize1 = ffsize + fftnamerr (0, fftname[ffx]);
	    ffsize_overflow |= (ffsize1 < ffsize);
	    ffsize = ffsize1;
	    fffmt = ffstpcpy (fffmt, ffprefix);
	    ffprefix = ffor;
	  }

      fff = FF_(ffformat);
      ffsize1 = ffsize + ffstrlen (fff);
      ffsize_overflow |= (ffsize1 < ffsize);
      ffsize = ffsize1;

      if (ffsize_overflow)
	return FFSIZE_MAXIMUM;

      if (ffresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *ffp = ffresult;
	  int ffi = 0;
	  while ((*ffp = *fff) != '\0')
	    {
	      if (*ffp == '%' && fff[1] == 's' && ffi < ffcount)
		{
		  ffp += fftnamerr (ffp, ffarg[ffi++]);
		  fff += 2;
		}
	      else
		{
		  ffp++;
		  fff++;
		}
	    }
	}
      return ffsize;
    }
}
#endif /* FFERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
ffdestruct (const char *ffmsg, int fftype, FFSTYPE *ffvaluep)
#else
static void
ffdestruct (ffmsg, fftype, ffvaluep)
    const char *ffmsg;
    int fftype;
    FFSTYPE *ffvaluep;
#endif
{
  FFUSE (ffvaluep);

  if (!ffmsg)
    ffmsg = "Deleting";
  FF_SYMBOL_PRINT (ffmsg, fftype, ffvaluep, fflocationp);

  switch (fftype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef FFPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int ffparse (void *FFPARSE_PARAM);
#else
int ffparse ();
#endif
#else /* ! FFPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int ffparse (void);
#else
int ffparse ();
#endif
#endif /* ! FFPARSE_PARAM */


/* The lookahead symbol.  */
int ffchar;

/* The semantic value of the lookahead symbol.  */
FFSTYPE fflval;

/* Number of syntax errors so far.  */
int ffnerrs;



/*-------------------------.
| ffparse or ffpush_parse.  |
`-------------------------*/

#ifdef FFPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
ffparse (void *FFPARSE_PARAM)
#else
int
ffparse (FFPARSE_PARAM)
    void *FFPARSE_PARAM;
#endif
#else /* ! FFPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
ffparse (void)
#else
int
ffparse ()

#endif
#endif
{


    int ffstate;
    /* Number of tokens to shift before error messages enabled.  */
    int fferrstatus;

    /* The stacks and their tools:
       `ffss': related to states.
       `ffvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow ffoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    fftype_int16 ffssa[FFINITDEPTH];
    fftype_int16 *ffss;
    fftype_int16 *ffssp;

    /* The semantic value stack.  */
    FFSTYPE ffvsa[FFINITDEPTH];
    FFSTYPE *ffvs;
    FFSTYPE *ffvsp;

    FFSIZE_T ffstacksize;

  int ffn;
  int ffresult;
  /* Lookahead token as an internal (translated) token number.  */
  int fftoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  FFSTYPE ffval;

#if FFERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char ffmsgbuf[128];
  char *ffmsg = ffmsgbuf;
  FFSIZE_T ffmsg_alloc = sizeof ffmsgbuf;
#endif

#define FFPOPSTACK(N)   (ffvsp -= (N), ffssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int fflen = 0;

  fftoken = 0;
  ffss = ffssa;
  ffvs = ffvsa;
  ffstacksize = FFINITDEPTH;

  FFDPRINTF ((stderr, "Starting parse\n"));

  ffstate = 0;
  fferrstatus = 0;
  ffnerrs = 0;
  ffchar = FFEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  ffssp = ffss;
  ffvsp = ffvs;

  goto ffsetstate;

/*------------------------------------------------------------.
| ffnewstate -- Push a new state, which is found in ffstate.  |
`------------------------------------------------------------*/
 ffnewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  ffssp++;

 ffsetstate:
  *ffssp = ffstate;

  if (ffss + ffstacksize - 1 <= ffssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      FFSIZE_T ffsize = ffssp - ffss + 1;

#ifdef ffoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	FFSTYPE *ffvs1 = ffvs;
	fftype_int16 *ffss1 = ffss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if ffoverflow is a macro.  */
	ffoverflow (FF_("memory exhausted"),
		    &ffss1, ffsize * sizeof (*ffssp),
		    &ffvs1, ffsize * sizeof (*ffvsp),
		    &ffstacksize);

	ffss = ffss1;
	ffvs = ffvs1;
      }
#else /* no ffoverflow */
# ifndef FFSTACK_RELOCATE
      goto ffexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (FFMAXDEPTH <= ffstacksize)
	goto ffexhaustedlab;
      ffstacksize *= 2;
      if (FFMAXDEPTH < ffstacksize)
	ffstacksize = FFMAXDEPTH;

      {
	fftype_int16 *ffss1 = ffss;
	union ffalloc *ffptr =
	  (union ffalloc *) FFSTACK_ALLOC (FFSTACK_BYTES (ffstacksize));
	if (! ffptr)
	  goto ffexhaustedlab;
	FFSTACK_RELOCATE (ffss_alloc, ffss);
	FFSTACK_RELOCATE (ffvs_alloc, ffvs);
#  undef FFSTACK_RELOCATE
	if (ffss1 != ffssa)
	  FFSTACK_FREE (ffss1);
      }
# endif
#endif /* no ffoverflow */

      ffssp = ffss + ffsize - 1;
      ffvsp = ffvs + ffsize - 1;

      FFDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) ffstacksize));

      if (ffss + ffstacksize - 1 <= ffssp)
	FFABORT;
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
    }
#endif /* !defined ffoverflow && !defined FFSTACK_RELOCATE */

<<<<<<< HEAD
=======
  FFDPRINTF ((stderr, "Entering state %d\n", ffstate));

>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
  if (ffstate == FFFINAL)
    FFACCEPT;

  goto ffbackup;

<<<<<<< HEAD

=======
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
/*-----------.
| ffbackup.  |
`-----------*/
ffbackup:
<<<<<<< HEAD
=======

>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  ffn = ffpact[ffstate];
<<<<<<< HEAD
  if (ffpact_value_is_default (ffn))
=======
  if (ffn == FFPACT_NINF)
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
    goto ffdefault;

  /* Not known => get a lookahead token if don't already have one.  */

<<<<<<< HEAD
  /* FFCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (ffchar == FFEMPTY)
    {
      FFDPRINTF ((stderr, "Reading a token\n"));
      ffchar = fflex ();
=======
  /* FFCHAR is either FFEMPTY or FFEOF or a valid lookahead symbol.  */
  if (ffchar == FFEMPTY)
    {
      FFDPRINTF ((stderr, "Reading a token: "));
      ffchar = FFLEX;
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
    }

  if (ffchar <= FFEOF)
    {
<<<<<<< HEAD
      ffchar = FFEOF;
      fftoken = FFSYMBOL_FFEOF;
      FFDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (ffchar == FFerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      ffchar = FFUNDEF;
      fftoken = FFSYMBOL_FFerror;
      goto fferrlab1;
=======
      ffchar = fftoken = FFEOF;
      FFDPRINTF ((stderr, "Now at end of input.\n"));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
    }
  else
    {
      fftoken = FFTRANSLATE (ffchar);
      FF_SYMBOL_PRINT ("Next token is", fftoken, &fflval, &fflloc);
    }

  /* If the proper action on seeing token FFTOKEN is to reduce or to
     detect an error, take that action.  */
  ffn += fftoken;
  if (ffn < 0 || FFLAST < ffn || ffcheck[ffn] != fftoken)
    goto ffdefault;
  ffn = fftable[ffn];
  if (ffn <= 0)
    {
<<<<<<< HEAD
      if (fftable_value_is_error (ffn))
        goto fferrlab;
=======
      if (ffn == 0 || ffn == FFTABLE_NINF)
	goto fferrlab;
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
      ffn = -ffn;
      goto ffreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (fferrstatus)
    fferrstatus--;

  /* Shift the lookahead token.  */
  FF_SYMBOL_PRINT ("Shifting", fftoken, &fflval, &fflloc);
<<<<<<< HEAD
  ffstate = ffn;
  FF_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++ffvsp = fflval;
  FF_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  ffchar = FFEMPTY;
=======

  /* Discard the shifted token.  */
  ffchar = FFEMPTY;

  ffstate = ffn;
  *++ffvsp = fflval;

>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
  goto ffnewstate;


/*-----------------------------------------------------------.
| ffdefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
ffdefault:
  ffn = ffdefact[ffstate];
  if (ffn == 0)
    goto fferrlab;
  goto ffreduce;


/*-----------------------------.
<<<<<<< HEAD
| ffreduce -- do a reduction.  |
=======
| ffreduce -- Do a reduction.  |
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
`-----------------------------*/
ffreduce:
  /* ffn is the number of a rule to reduce with.  */
  fflen = ffr2[ffn];

  /* If FFLEN is nonzero, implement the default value of the action:
<<<<<<< HEAD
     '$$ = $1'.
=======
     `$$ = $1'.
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0

     Otherwise, the following line sets FFVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to FFVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that FFVAL may be used uninitialized.  */
  ffval = ffvsp[1-fflen];


  FF_REDUCE_PRINT (ffn);
  switch (ffn)
    {
<<<<<<< HEAD
  case 4: /* line: '\n'  */
#line 256 "eval.y"
                     {}
#line 1913 "y.tab.c"
    break;

  case 5: /* line: expr '\n'  */
#line 258 "eval.y"
                { if( (ffvsp[-1].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
                  gParse.resultNode = (ffvsp[-1].Node);
		}
#line 1923 "y.tab.c"
    break;

  case 6: /* line: bexpr '\n'  */
#line 264 "eval.y"
                { if( (ffvsp[-1].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
                  gParse.resultNode = (ffvsp[-1].Node);
		}
#line 1933 "y.tab.c"
    break;

  case 7: /* line: sexpr '\n'  */
#line 270 "eval.y"
                { if( (ffvsp[-1].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  } 
                  gParse.resultNode = (ffvsp[-1].Node);
		}
#line 1943 "y.tab.c"
    break;

  case 8: /* line: bits '\n'  */
#line 276 "eval.y"
                { if( (ffvsp[-1].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
                  gParse.resultNode = (ffvsp[-1].Node);
		}
#line 1953 "y.tab.c"
    break;

  case 9: /* line: error '\n'  */
#line 281 "eval.y"
                     {  fferrok;  }
#line 1959 "y.tab.c"
    break;

  case 10: /* bvector: '{' bexpr  */
#line 285 "eval.y"
                { (ffval.Node) = New_Vector( (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 1965 "y.tab.c"
    break;

  case 11: /* bvector: bvector ',' bexpr  */
#line 287 "eval.y"
                {
                  if( gParse.Nodes[(ffvsp[-2].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[-2].Node) = Close_Vec( (ffvsp[-2].Node) ); TEST((ffvsp[-2].Node));
		     (ffval.Node) = New_Vector( (ffvsp[-2].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[-2].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[0].Node);
                }
#line 1980 "y.tab.c"
    break;

  case 12: /* vector: '{' expr  */
#line 300 "eval.y"
                { (ffval.Node) = New_Vector( (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 1986 "y.tab.c"
    break;

  case 13: /* vector: vector ',' expr  */
#line 302 "eval.y"
                {
                  if( TYPE((ffvsp[-2].Node)) < TYPE((ffvsp[0].Node)) )
                     TYPE((ffvsp[-2].Node)) = TYPE((ffvsp[0].Node));
                  if( gParse.Nodes[(ffvsp[-2].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[-2].Node) = Close_Vec( (ffvsp[-2].Node) ); TEST((ffvsp[-2].Node));
		     (ffval.Node) = New_Vector( (ffvsp[-2].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[-2].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[0].Node);
                }
#line 2003 "y.tab.c"
    break;

  case 14: /* vector: vector ',' bexpr  */
#line 315 "eval.y"
                {
                  if( gParse.Nodes[(ffvsp[-2].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[-2].Node) = Close_Vec( (ffvsp[-2].Node) ); TEST((ffvsp[-2].Node));
		     (ffval.Node) = New_Vector( (ffvsp[-2].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[-2].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[0].Node);
                }
#line 2018 "y.tab.c"
    break;

  case 15: /* vector: bvector ',' expr  */
#line 326 "eval.y"
                {
                  TYPE((ffvsp[-2].Node)) = TYPE((ffvsp[0].Node));
                  if( gParse.Nodes[(ffvsp[-2].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[-2].Node) = Close_Vec( (ffvsp[-2].Node) ); TEST((ffvsp[-2].Node));
		     (ffval.Node) = New_Vector( (ffvsp[-2].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[-2].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[0].Node);
                }
#line 2034 "y.tab.c"
    break;

  case 16: /* expr: vector '}'  */
#line 340 "eval.y"
                { (ffval.Node) = Close_Vec( (ffvsp[-1].Node) ); TEST((ffval.Node)); }
#line 2040 "y.tab.c"
    break;

  case 17: /* bexpr: bvector '}'  */
#line 344 "eval.y"
                { (ffval.Node) = Close_Vec( (ffvsp[-1].Node) ); TEST((ffval.Node)); }
#line 2046 "y.tab.c"
    break;

  case 18: /* bits: BITSTR  */
#line 348 "eval.y"
                {
                  (ffval.Node) = New_Const( BITSTR, (ffvsp[0].str), strlen((ffvsp[0].str))+1 ); TEST((ffval.Node));
		  SIZE((ffval.Node)) = strlen((ffvsp[0].str)); }
#line 2054 "y.tab.c"
    break;

  case 19: /* bits: BITCOL  */
#line 352 "eval.y"
                { (ffval.Node) = New_Column( (ffvsp[0].lng) ); TEST((ffval.Node)); }
#line 2060 "y.tab.c"
    break;

  case 20: /* bits: BITCOL '{' expr '}'  */
#line 354 "eval.y"
                {
                  if( TYPE((ffvsp[-1].Node)) != LONG
		      || OPER((ffvsp[-1].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[-3].lng), (ffvsp[-1].Node) ); TEST((ffval.Node));
                }
#line 2073 "y.tab.c"
    break;

  case 21: /* bits: bits '&' bits  */
#line 363 "eval.y"
                { (ffval.Node) = New_BinOp( BITSTR, (ffvsp[-2].Node), '&', (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = ( SIZE((ffvsp[-2].Node))>SIZE((ffvsp[0].Node)) ? SIZE((ffvsp[-2].Node)) : SIZE((ffvsp[0].Node)) );  }
#line 2080 "y.tab.c"
    break;

  case 22: /* bits: bits '|' bits  */
#line 366 "eval.y"
                { (ffval.Node) = New_BinOp( BITSTR, (ffvsp[-2].Node), '|', (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = ( SIZE((ffvsp[-2].Node))>SIZE((ffvsp[0].Node)) ? SIZE((ffvsp[-2].Node)) : SIZE((ffvsp[0].Node)) );  }
#line 2087 "y.tab.c"
    break;

  case 23: /* bits: bits '+' bits  */
#line 369 "eval.y"
                { 
		  if (SIZE((ffvsp[-2].Node))+SIZE((ffvsp[0].Node)) >= MAX_STRLEN) {
		    fferror("Combined bit string size exceeds " MAX_STRLEN_S " bits");
		    FFERROR;
		  }
		  (ffval.Node) = New_BinOp( BITSTR, (ffvsp[-2].Node), '+', (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = SIZE((ffvsp[-2].Node)) + SIZE((ffvsp[0].Node)); 
		}
#line 2100 "y.tab.c"
    break;

  case 24: /* bits: bits '[' expr ']'  */
#line 378 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-3].Node), 1, (ffvsp[-1].Node),  0,  0,  0,   0 ); TEST((ffval.Node)); }
#line 2106 "y.tab.c"
    break;

  case 25: /* bits: bits '[' expr ',' expr ']'  */
#line 380 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-5].Node), 2, (ffvsp[-3].Node), (ffvsp[-1].Node),  0,  0,   0 ); TEST((ffval.Node)); }
#line 2112 "y.tab.c"
    break;

  case 26: /* bits: bits '[' expr ',' expr ',' expr ']'  */
#line 382 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-7].Node), 3, (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),  0,   0 ); TEST((ffval.Node)); }
#line 2118 "y.tab.c"
    break;

  case 27: /* bits: bits '[' expr ',' expr ',' expr ',' expr ']'  */
#line 384 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-9].Node), 4, (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),   0 ); TEST((ffval.Node)); }
#line 2124 "y.tab.c"
    break;

  case 28: /* bits: bits '[' expr ',' expr ',' expr ',' expr ',' expr ']'  */
#line 386 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-11].Node), 5, (ffvsp[-9].Node), (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node) ); TEST((ffval.Node)); }
#line 2130 "y.tab.c"
    break;

  case 29: /* bits: NOT bits  */
#line 388 "eval.y"
                { (ffval.Node) = New_Unary( BITSTR, NOT, (ffvsp[0].Node) ); TEST((ffval.Node));     }
#line 2136 "y.tab.c"
    break;

  case 30: /* bits: '(' bits ')'  */
#line 391 "eval.y"
                { (ffval.Node) = (ffvsp[-1].Node); }
#line 2142 "y.tab.c"
    break;

  case 31: /* expr: LONG  */
#line 395 "eval.y"
                { (ffval.Node) = New_Const( LONG,   &((ffvsp[0].lng)), sizeof(long)   ); TEST((ffval.Node)); }
#line 2148 "y.tab.c"
    break;

  case 32: /* expr: DOUBLE  */
#line 397 "eval.y"
                { (ffval.Node) = New_Const( DOUBLE, &((ffvsp[0].dbl)), sizeof(double) ); TEST((ffval.Node)); }
#line 2154 "y.tab.c"
    break;

  case 33: /* expr: COLUMN  */
#line 399 "eval.y"
                { (ffval.Node) = New_Column( (ffvsp[0].lng) ); TEST((ffval.Node)); }
#line 2160 "y.tab.c"
    break;

  case 34: /* expr: COLUMN '{' expr '}'  */
#line 401 "eval.y"
                {
                  if( TYPE((ffvsp[-1].Node)) != LONG
		      || OPER((ffvsp[-1].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[-3].lng), (ffvsp[-1].Node) ); TEST((ffval.Node));
                }
#line 2173 "y.tab.c"
    break;

  case 35: /* expr: ROWREF  */
#line 410 "eval.y"
                { (ffval.Node) = New_Func( LONG, row_fct,  0, 0, 0, 0, 0, 0, 0, 0 ); }
#line 2179 "y.tab.c"
    break;

  case 36: /* expr: NULLREF  */
#line 412 "eval.y"
                { (ffval.Node) = New_Func( LONG, null_fct, 0, 0, 0, 0, 0, 0, 0, 0 ); }
#line 2185 "y.tab.c"
    break;

  case 37: /* expr: expr '%' expr  */
#line 414 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '%', (ffvsp[0].Node) );
		  TEST((ffval.Node));                                                }
#line 2192 "y.tab.c"
    break;

  case 38: /* expr: expr '+' expr  */
#line 417 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '+', (ffvsp[0].Node) );
		  TEST((ffval.Node));                                                }
#line 2199 "y.tab.c"
    break;

  case 39: /* expr: expr '-' expr  */
#line 420 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '-', (ffvsp[0].Node) ); 
		  TEST((ffval.Node));                                                }
#line 2206 "y.tab.c"
    break;

  case 40: /* expr: expr '*' expr  */
#line 423 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '*', (ffvsp[0].Node) ); 
		  TEST((ffval.Node));                                                }
#line 2213 "y.tab.c"
    break;

  case 41: /* expr: expr '/' expr  */
#line 426 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '/', (ffvsp[0].Node) ); 
		  TEST((ffval.Node));                                                }
#line 2220 "y.tab.c"
    break;

  case 42: /* expr: expr '&' expr  */
#line 429 "eval.y"
                { 
                   if (TYPE((ffvsp[-2].Node)) != LONG ||
		       TYPE((ffvsp[0].Node)) != LONG) {
                     fferror("Bitwise operations with incompatible types; only (bit OP bit) and (int OP int) are allowed");
                      FFERROR;
                   }
                   (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '&', (ffvsp[0].Node) );
                }
#line 2233 "y.tab.c"
    break;

  case 43: /* expr: expr '|' expr  */
#line 438 "eval.y"
                { 
                   if (TYPE((ffvsp[-2].Node)) != LONG ||
		       TYPE((ffvsp[0].Node)) != LONG) {
                     fferror("Bitwise operations with incompatible types; only (bit OP bit) and (int OP int) are allowed");
                      FFERROR;
                   }
                   (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '|', (ffvsp[0].Node) );
                }
#line 2246 "y.tab.c"
    break;

  case 44: /* expr: expr XOR expr  */
#line 447 "eval.y"
                { 
                   if (TYPE((ffvsp[-2].Node)) != LONG ||
		       TYPE((ffvsp[0].Node)) != LONG) {
                     fferror("Bitwise operations with incompatible types; only (bit OP bit) and (int OP int) are allowed");
                      FFERROR;
                   }
                   (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '^', (ffvsp[0].Node) );
                }
#line 2259 "y.tab.c"
    break;

  case 45: /* expr: expr POWER expr  */
#line 456 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), POWER, (ffvsp[0].Node) );
		  TEST((ffval.Node));                                                }
#line 2266 "y.tab.c"
    break;

  case 46: /* expr: '+' expr  */
#line 459 "eval.y"
                { (ffval.Node) = (ffvsp[0].Node); }
#line 2272 "y.tab.c"
    break;

  case 47: /* expr: '-' expr  */
#line 461 "eval.y"
                { (ffval.Node) = New_Unary( TYPE((ffvsp[0].Node)), UMINUS, (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 2278 "y.tab.c"
    break;

  case 48: /* expr: '(' expr ')'  */
#line 463 "eval.y"
                { (ffval.Node) = (ffvsp[-1].Node); }
#line 2284 "y.tab.c"
    break;

  case 49: /* expr: expr '*' bexpr  */
#line 465 "eval.y"
                { (ffvsp[0].Node) = New_Unary( TYPE((ffvsp[-2].Node)), 0, (ffvsp[0].Node) );
                  (ffval.Node) = New_BinOp( TYPE((ffvsp[-2].Node)), (ffvsp[-2].Node), '*', (ffvsp[0].Node) ); 
		  TEST((ffval.Node));                                }
#line 2292 "y.tab.c"
    break;

  case 50: /* expr: bexpr '*' expr  */
#line 469 "eval.y"
                { (ffvsp[-2].Node) = New_Unary( TYPE((ffvsp[0].Node)), 0, (ffvsp[-2].Node) );
                  (ffval.Node) = New_BinOp( TYPE((ffvsp[0].Node)), (ffvsp[-2].Node), '*', (ffvsp[0].Node) );
                  TEST((ffval.Node));                                }
#line 2300 "y.tab.c"
    break;

  case 51: /* expr: bexpr '?' expr ':' expr  */
#line 473 "eval.y"
                {
                  PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node));
                  if( ! Test_Dims((ffvsp[-2].Node),(ffvsp[0].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[-2].Node), (ffvsp[0].Node), (ffvsp[-4].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[-2].Node))<SIZE((ffvsp[0].Node)) )  Copy_Dims((ffval.Node), (ffvsp[0].Node));
                  TYPE((ffvsp[-4].Node)) = TYPE((ffvsp[-2].Node));
                  if( ! Test_Dims((ffvsp[-4].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  TYPE((ffvsp[-4].Node)) = BOOLEAN;
                  if( SIZE((ffval.Node))<SIZE((ffvsp[-4].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-4].Node));
                }
#line 2323 "y.tab.c"
    break;

  case 52: /* expr: bexpr '?' bexpr ':' expr  */
#line 492 "eval.y"
                {
                  PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node));
                  if( ! Test_Dims((ffvsp[-2].Node),(ffvsp[0].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[-2].Node), (ffvsp[0].Node), (ffvsp[-4].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[-2].Node))<SIZE((ffvsp[0].Node)) )  Copy_Dims((ffval.Node), (ffvsp[0].Node));
                  TYPE((ffvsp[-4].Node)) = TYPE((ffvsp[-2].Node));
                  if( ! Test_Dims((ffvsp[-4].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  TYPE((ffvsp[-4].Node)) = BOOLEAN;
                  if( SIZE((ffval.Node))<SIZE((ffvsp[-4].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-4].Node));
                }
#line 2346 "y.tab.c"
    break;

  case 53: /* expr: bexpr '?' expr ':' bexpr  */
#line 511 "eval.y"
                {
                  PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node));
                  if( ! Test_Dims((ffvsp[-2].Node),(ffvsp[0].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[-2].Node), (ffvsp[0].Node), (ffvsp[-4].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[-2].Node))<SIZE((ffvsp[0].Node)) )  Copy_Dims((ffval.Node), (ffvsp[0].Node));
                  TYPE((ffvsp[-4].Node)) = TYPE((ffvsp[-2].Node));
                  if( ! Test_Dims((ffvsp[-4].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  TYPE((ffvsp[-4].Node)) = BOOLEAN;
                  if( SIZE((ffval.Node))<SIZE((ffvsp[-4].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-4].Node));
                }
#line 2369 "y.tab.c"
    break;

  case 54: /* expr: FUNCTION ')'  */
#line 530 "eval.y"
                { if (FSTRCMP((ffvsp[-1].str),"RANDOM(") == 0) {  /* Scalar RANDOM() */
                     (ffval.Node) = New_Func( DOUBLE, rnd_fct, 0, 0, 0, 0, 0, 0, 0, 0 );
		  } else if (FSTRCMP((ffvsp[-1].str),"RANDOMN(") == 0) {/*Scalar RANDOMN()*/
=======
        case 4:

/* Line 1455 of yacc.c  */
#line 248 "eval.y"
    {}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 250 "eval.y"
    { if( (ffvsp[(1) - (2)].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
                  gParse.resultNode = (ffvsp[(1) - (2)].Node);
		}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 256 "eval.y"
    { if( (ffvsp[(1) - (2)].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
                  gParse.resultNode = (ffvsp[(1) - (2)].Node);
		}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 262 "eval.y"
    { if( (ffvsp[(1) - (2)].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  } 
                  gParse.resultNode = (ffvsp[(1) - (2)].Node);
		}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 268 "eval.y"
    { if( (ffvsp[(1) - (2)].Node)<0 ) {
		     fferror("Couldn't build node structure: out of memory?");
		     FFERROR;  }
                  gParse.resultNode = (ffvsp[(1) - (2)].Node);
		}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 273 "eval.y"
    {  fferrok;  }
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 277 "eval.y"
    { (ffval.Node) = New_Vector( (ffvsp[(2) - (2)].Node) ); TEST((ffval.Node)); }
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 279 "eval.y"
    {
                  if( gParse.Nodes[(ffvsp[(1) - (3)].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[(1) - (3)].Node) = Close_Vec( (ffvsp[(1) - (3)].Node) ); TEST((ffvsp[(1) - (3)].Node));
		     (ffval.Node) = New_Vector( (ffvsp[(1) - (3)].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[(1) - (3)].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[(3) - (3)].Node);
                }
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 292 "eval.y"
    { (ffval.Node) = New_Vector( (ffvsp[(2) - (2)].Node) ); TEST((ffval.Node)); }
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 294 "eval.y"
    {
                  if( TYPE((ffvsp[(1) - (3)].Node)) < TYPE((ffvsp[(3) - (3)].Node)) )
                     TYPE((ffvsp[(1) - (3)].Node)) = TYPE((ffvsp[(3) - (3)].Node));
                  if( gParse.Nodes[(ffvsp[(1) - (3)].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[(1) - (3)].Node) = Close_Vec( (ffvsp[(1) - (3)].Node) ); TEST((ffvsp[(1) - (3)].Node));
		     (ffval.Node) = New_Vector( (ffvsp[(1) - (3)].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[(1) - (3)].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[(3) - (3)].Node);
                }
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 307 "eval.y"
    {
                  if( gParse.Nodes[(ffvsp[(1) - (3)].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[(1) - (3)].Node) = Close_Vec( (ffvsp[(1) - (3)].Node) ); TEST((ffvsp[(1) - (3)].Node));
		     (ffval.Node) = New_Vector( (ffvsp[(1) - (3)].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[(1) - (3)].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[(3) - (3)].Node);
                }
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 318 "eval.y"
    {
                  TYPE((ffvsp[(1) - (3)].Node)) = TYPE((ffvsp[(3) - (3)].Node));
                  if( gParse.Nodes[(ffvsp[(1) - (3)].Node)].nSubNodes >= MAXSUBS ) {
		     (ffvsp[(1) - (3)].Node) = Close_Vec( (ffvsp[(1) - (3)].Node) ); TEST((ffvsp[(1) - (3)].Node));
		     (ffval.Node) = New_Vector( (ffvsp[(1) - (3)].Node) ); TEST((ffval.Node));
                  } else {
                     (ffval.Node) = (ffvsp[(1) - (3)].Node);
                  }
		  gParse.Nodes[(ffval.Node)].SubNodes[ gParse.Nodes[(ffval.Node)].nSubNodes++ ]
		     = (ffvsp[(3) - (3)].Node);
                }
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 332 "eval.y"
    { (ffval.Node) = Close_Vec( (ffvsp[(1) - (2)].Node) ); TEST((ffval.Node)); }
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 336 "eval.y"
    { (ffval.Node) = Close_Vec( (ffvsp[(1) - (2)].Node) ); TEST((ffval.Node)); }
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 340 "eval.y"
    {
                  (ffval.Node) = New_Const( BITSTR, (ffvsp[(1) - (1)].str), strlen((ffvsp[(1) - (1)].str))+1 ); TEST((ffval.Node));
		  SIZE((ffval.Node)) = strlen((ffvsp[(1) - (1)].str)); }
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 344 "eval.y"
    { (ffval.Node) = New_Column( (ffvsp[(1) - (1)].lng) ); TEST((ffval.Node)); }
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 346 "eval.y"
    {
                  if( TYPE((ffvsp[(3) - (4)].Node)) != LONG
		      || OPER((ffvsp[(3) - (4)].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[(1) - (4)].lng), (ffvsp[(3) - (4)].Node) ); TEST((ffval.Node));
                }
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 355 "eval.y"
    { (ffval.Node) = New_BinOp( BITSTR, (ffvsp[(1) - (3)].Node), '&', (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = ( SIZE((ffvsp[(1) - (3)].Node))>SIZE((ffvsp[(3) - (3)].Node)) ? SIZE((ffvsp[(1) - (3)].Node)) : SIZE((ffvsp[(3) - (3)].Node)) );  }
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 358 "eval.y"
    { (ffval.Node) = New_BinOp( BITSTR, (ffvsp[(1) - (3)].Node), '|', (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = ( SIZE((ffvsp[(1) - (3)].Node))>SIZE((ffvsp[(3) - (3)].Node)) ? SIZE((ffvsp[(1) - (3)].Node)) : SIZE((ffvsp[(3) - (3)].Node)) );  }
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 361 "eval.y"
    { 
		  if (SIZE((ffvsp[(1) - (3)].Node))+SIZE((ffvsp[(3) - (3)].Node)) >= MAX_STRLEN) {
		    fferror("Combined bit string size exceeds " MAX_STRLEN_S " bits");
		    FFERROR;
		  }
		  (ffval.Node) = New_BinOp( BITSTR, (ffvsp[(1) - (3)].Node), '+', (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = SIZE((ffvsp[(1) - (3)].Node)) + SIZE((ffvsp[(3) - (3)].Node)); 
		}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 370 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (4)].Node), 1, (ffvsp[(3) - (4)].Node),  0,  0,  0,   0 ); TEST((ffval.Node)); }
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 372 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (6)].Node), 2, (ffvsp[(3) - (6)].Node), (ffvsp[(5) - (6)].Node),  0,  0,   0 ); TEST((ffval.Node)); }
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 374 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (8)].Node), 3, (ffvsp[(3) - (8)].Node), (ffvsp[(5) - (8)].Node), (ffvsp[(7) - (8)].Node),  0,   0 ); TEST((ffval.Node)); }
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 376 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (10)].Node), 4, (ffvsp[(3) - (10)].Node), (ffvsp[(5) - (10)].Node), (ffvsp[(7) - (10)].Node), (ffvsp[(9) - (10)].Node),   0 ); TEST((ffval.Node)); }
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 378 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (12)].Node), 5, (ffvsp[(3) - (12)].Node), (ffvsp[(5) - (12)].Node), (ffvsp[(7) - (12)].Node), (ffvsp[(9) - (12)].Node), (ffvsp[(11) - (12)].Node) ); TEST((ffval.Node)); }
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 380 "eval.y"
    { (ffval.Node) = New_Unary( BITSTR, NOT, (ffvsp[(2) - (2)].Node) ); TEST((ffval.Node));     }
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 383 "eval.y"
    { (ffval.Node) = (ffvsp[(2) - (3)].Node); }
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 387 "eval.y"
    { (ffval.Node) = New_Const( LONG,   &((ffvsp[(1) - (1)].lng)), sizeof(long)   ); TEST((ffval.Node)); }
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 389 "eval.y"
    { (ffval.Node) = New_Const( DOUBLE, &((ffvsp[(1) - (1)].dbl)), sizeof(double) ); TEST((ffval.Node)); }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 391 "eval.y"
    { (ffval.Node) = New_Column( (ffvsp[(1) - (1)].lng) ); TEST((ffval.Node)); }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 393 "eval.y"
    {
                  if( TYPE((ffvsp[(3) - (4)].Node)) != LONG
		      || OPER((ffvsp[(3) - (4)].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[(1) - (4)].lng), (ffvsp[(3) - (4)].Node) ); TEST((ffval.Node));
                }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 402 "eval.y"
    { (ffval.Node) = New_Func( LONG, row_fct,  0, 0, 0, 0, 0, 0, 0, 0 ); }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 404 "eval.y"
    { (ffval.Node) = New_Func( LONG, null_fct, 0, 0, 0, 0, 0, 0, 0, 0 ); }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 406 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[(1) - (3)].Node)), (ffvsp[(1) - (3)].Node), '%', (ffvsp[(3) - (3)].Node) );
		  TEST((ffval.Node));                                                }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 409 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[(1) - (3)].Node)), (ffvsp[(1) - (3)].Node), '+', (ffvsp[(3) - (3)].Node) );
		  TEST((ffval.Node));                                                }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 412 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[(1) - (3)].Node)), (ffvsp[(1) - (3)].Node), '-', (ffvsp[(3) - (3)].Node) ); 
		  TEST((ffval.Node));                                                }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 415 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[(1) - (3)].Node)), (ffvsp[(1) - (3)].Node), '*', (ffvsp[(3) - (3)].Node) ); 
		  TEST((ffval.Node));                                                }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 418 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[(1) - (3)].Node)), (ffvsp[(1) - (3)].Node), '/', (ffvsp[(3) - (3)].Node) ); 
		  TEST((ffval.Node));                                                }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 421 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( TYPE((ffvsp[(1) - (3)].Node)), (ffvsp[(1) - (3)].Node), POWER, (ffvsp[(3) - (3)].Node) );
		  TEST((ffval.Node));                                                }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 424 "eval.y"
    { (ffval.Node) = (ffvsp[(2) - (2)].Node); }
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 426 "eval.y"
    { (ffval.Node) = New_Unary( TYPE((ffvsp[(2) - (2)].Node)), UMINUS, (ffvsp[(2) - (2)].Node) ); TEST((ffval.Node)); }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 428 "eval.y"
    { (ffval.Node) = (ffvsp[(2) - (3)].Node); }
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 430 "eval.y"
    { (ffvsp[(3) - (3)].Node) = New_Unary( TYPE((ffvsp[(1) - (3)].Node)), 0, (ffvsp[(3) - (3)].Node) );
                  (ffval.Node) = New_BinOp( TYPE((ffvsp[(1) - (3)].Node)), (ffvsp[(1) - (3)].Node), '*', (ffvsp[(3) - (3)].Node) ); 
		  TEST((ffval.Node));                                }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 434 "eval.y"
    { (ffvsp[(1) - (3)].Node) = New_Unary( TYPE((ffvsp[(3) - (3)].Node)), 0, (ffvsp[(1) - (3)].Node) );
                  (ffval.Node) = New_BinOp( TYPE((ffvsp[(3) - (3)].Node)), (ffvsp[(1) - (3)].Node), '*', (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                }
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 438 "eval.y"
    {
                  PROMOTE((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node));
                  if( ! Test_Dims((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[(3) - (5)].Node), (ffvsp[(5) - (5)].Node), (ffvsp[(1) - (5)].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[(3) - (5)].Node))<SIZE((ffvsp[(5) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(5) - (5)].Node));
                  TYPE((ffvsp[(1) - (5)].Node)) = TYPE((ffvsp[(3) - (5)].Node));
                  if( ! Test_Dims((ffvsp[(1) - (5)].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  TYPE((ffvsp[(1) - (5)].Node)) = BOOLEAN;
                  if( SIZE((ffval.Node))<SIZE((ffvsp[(1) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(1) - (5)].Node));
                }
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 457 "eval.y"
    {
                  PROMOTE((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node));
                  if( ! Test_Dims((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[(3) - (5)].Node), (ffvsp[(5) - (5)].Node), (ffvsp[(1) - (5)].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[(3) - (5)].Node))<SIZE((ffvsp[(5) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(5) - (5)].Node));
                  TYPE((ffvsp[(1) - (5)].Node)) = TYPE((ffvsp[(3) - (5)].Node));
                  if( ! Test_Dims((ffvsp[(1) - (5)].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  TYPE((ffvsp[(1) - (5)].Node)) = BOOLEAN;
                  if( SIZE((ffval.Node))<SIZE((ffvsp[(1) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(1) - (5)].Node));
                }
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 476 "eval.y"
    {
                  PROMOTE((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node));
                  if( ! Test_Dims((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[(3) - (5)].Node), (ffvsp[(5) - (5)].Node), (ffvsp[(1) - (5)].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[(3) - (5)].Node))<SIZE((ffvsp[(5) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(5) - (5)].Node));
                  TYPE((ffvsp[(1) - (5)].Node)) = TYPE((ffvsp[(3) - (5)].Node));
                  if( ! Test_Dims((ffvsp[(1) - (5)].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  TYPE((ffvsp[(1) - (5)].Node)) = BOOLEAN;
                  if( SIZE((ffval.Node))<SIZE((ffvsp[(1) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(1) - (5)].Node));
                }
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 495 "eval.y"
    { if (FSTRCMP((ffvsp[(1) - (2)].str),"RANDOM(") == 0) {  /* Scalar RANDOM() */
                     (ffval.Node) = New_Func( DOUBLE, rnd_fct, 0, 0, 0, 0, 0, 0, 0, 0 );
		  } else if (FSTRCMP((ffvsp[(1) - (2)].str),"RANDOMN(") == 0) {/*Scalar RANDOMN()*/
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     (ffval.Node) = New_Func( DOUBLE, gasrnd_fct, 0, 0, 0, 0, 0, 0, 0, 0 );
                  } else {
                     fferror("Function() not supported");
		     FFERROR;
		  }
                  TEST((ffval.Node)); 
                }
<<<<<<< HEAD
#line 2384 "y.tab.c"
    break;

  case 55: /* expr: FUNCTION bexpr ')'  */
#line 541 "eval.y"
                { if (FSTRCMP((ffvsp[-2].str),"SUM(") == 0) {
		     (ffval.Node) = New_Func( LONG, sum_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
                  } else if (FSTRCMP((ffvsp[-2].str),"NELEM(") == 0) {
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[-1].Node)) ), sizeof(long) );
                  } else if (FSTRCMP((ffvsp[-2].str),"ACCUM(") == 0) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG , (ffvsp[-1].Node), ACCUM, New_Const( LONG, &zero, sizeof(zero) ));
=======
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 506 "eval.y"
    { if (FSTRCMP((ffvsp[(1) - (3)].str),"SUM(") == 0) {
		     (ffval.Node) = New_Func( LONG, sum_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
                  } else if (FSTRCMP((ffvsp[(1) - (3)].str),"NELEM(") == 0) {
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[(2) - (3)].Node)) ), sizeof(long) );
                  } else if (FSTRCMP((ffvsp[(1) - (3)].str),"ACCUM(") == 0) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG , (ffvsp[(2) - (3)].Node), ACCUM, New_Const( LONG, &zero, sizeof(zero) ));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		  } else {
                     fferror("Function(bool) not supported");
		     FFERROR;
		  }
                  TEST((ffval.Node)); 
		}
<<<<<<< HEAD
#line 2402 "y.tab.c"
    break;

  case 56: /* expr: FUNCTION sexpr ')'  */
#line 555 "eval.y"
                { if (FSTRCMP((ffvsp[-2].str),"NELEM(") == 0) {
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[-1].Node)) ), sizeof(long) );
		  } else if (FSTRCMP((ffvsp[-2].str),"NVALID(") == 0) {
		     (ffval.Node) = New_Func( LONG, nonnull_fct, 1, (ffvsp[-1].Node),
=======
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 520 "eval.y"
    { if (FSTRCMP((ffvsp[(1) - (3)].str),"NELEM(") == 0) {
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[(2) - (3)].Node)) ), sizeof(long) );
		  } else if (FSTRCMP((ffvsp[(1) - (3)].str),"NVALID(") == 0) {
		     (ffval.Node) = New_Func( LONG, nonnull_fct, 1, (ffvsp[(2) - (3)].Node),
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
				    0, 0, 0, 0, 0, 0 );
		  } else {
                     fferror("Function(str) not supported");
		     FFERROR;
		  }
                  TEST((ffval.Node)); 
		}
<<<<<<< HEAD
#line 2418 "y.tab.c"
    break;

  case 57: /* expr: FUNCTION bits ')'  */
#line 567 "eval.y"
                { if (FSTRCMP((ffvsp[-2].str),"NELEM(") == 0) {
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[-1].Node)) ), sizeof(long) );
		} else if (FSTRCMP((ffvsp[-2].str),"NVALID(") == 0) { /* Bit arrays do not have NULL */
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[-1].Node)) ), sizeof(long) );
		} else if (FSTRCMP((ffvsp[-2].str),"SUM(") == 0) {
		     (ffval.Node) = New_Func( LONG, sum_fct, 1, (ffvsp[-1].Node),
				    0, 0, 0, 0, 0, 0 );
		} else if (FSTRCMP((ffvsp[-2].str),"MIN(") == 0) {
		     (ffval.Node) = New_Func( TYPE((ffvsp[-1].Node)),  /* Force 1D result */
				    min1_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
=======
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 532 "eval.y"
    { if (FSTRCMP((ffvsp[(1) - (3)].str),"NELEM(") == 0) {
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[(2) - (3)].Node)) ), sizeof(long) );
		} else if (FSTRCMP((ffvsp[(1) - (3)].str),"NVALID(") == 0) { /* Bit arrays do not have NULL */
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[(2) - (3)].Node)) ), sizeof(long) );
		} else if (FSTRCMP((ffvsp[(1) - (3)].str),"SUM(") == 0) {
		     (ffval.Node) = New_Func( LONG, sum_fct, 1, (ffvsp[(2) - (3)].Node),
				    0, 0, 0, 0, 0, 0 );
		} else if (FSTRCMP((ffvsp[(1) - (3)].str),"MIN(") == 0) {
		     (ffval.Node) = New_Func( TYPE((ffvsp[(2) - (3)].Node)),  /* Force 1D result */
				    min1_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     /* Note: $2 is a vector so the result can never
		        be a constant.  Therefore it will never be set
		        inside New_Func(), and it is safe to set SIZE() */
		     SIZE((ffval.Node)) = 1;
<<<<<<< HEAD
		} else if (FSTRCMP((ffvsp[-2].str),"ACCUM(") == 0) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG , (ffvsp[-1].Node), ACCUM, New_Const( LONG, &zero, sizeof(zero) ));
		} else if (FSTRCMP((ffvsp[-2].str),"MAX(") == 0) {
		     (ffval.Node) = New_Func( TYPE((ffvsp[-1].Node)),  /* Force 1D result */
				    max1_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
=======
		} else if (FSTRCMP((ffvsp[(1) - (3)].str),"ACCUM(") == 0) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG , (ffvsp[(2) - (3)].Node), ACCUM, New_Const( LONG, &zero, sizeof(zero) ));
		} else if (FSTRCMP((ffvsp[(1) - (3)].str),"MAX(") == 0) {
		     (ffval.Node) = New_Func( TYPE((ffvsp[(2) - (3)].Node)),  /* Force 1D result */
				    max1_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     /* Note: $2 is a vector so the result can never
		        be a constant.  Therefore it will never be set
		        inside New_Func(), and it is safe to set SIZE() */
		     SIZE((ffval.Node)) = 1;
		} else {
                     fferror("Function(bits) not supported");
		     FFERROR;
		  }
                  TEST((ffval.Node)); 
		}
<<<<<<< HEAD
#line 2453 "y.tab.c"
    break;

  case 58: /* expr: FUNCTION expr ')'  */
#line 598 "eval.y"
                { if (FSTRCMP((ffvsp[-2].str),"SUM(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[-1].Node)), sum_fct, 1, (ffvsp[-1].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[-2].str),"AVERAGE(") == 0)
		     (ffval.Node) = New_Func( DOUBLE, average_fct, 1, (ffvsp[-1].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[-2].str),"STDDEV(") == 0)
		     (ffval.Node) = New_Func( DOUBLE, stddev_fct, 1, (ffvsp[-1].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[-2].str),"MEDIAN(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[-1].Node)), median_fct, 1, (ffvsp[-1].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[-2].str),"NELEM(") == 0)
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[-1].Node)) ), sizeof(long) );
		  else if (FSTRCMP((ffvsp[-2].str),"NVALID(") == 0)
		     (ffval.Node) = New_Func( LONG, nonnull_fct, 1, (ffvsp[-1].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if   ((FSTRCMP((ffvsp[-2].str),"ACCUM(") == 0) && (TYPE((ffvsp[-1].Node)) == LONG)) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG ,   (ffvsp[-1].Node), ACCUM, New_Const( LONG,   &zero, sizeof(zero) ));
		  } else if ((FSTRCMP((ffvsp[-2].str),"ACCUM(") == 0) && (TYPE((ffvsp[-1].Node)) == DOUBLE)) {
		    double zero = 0;
		    (ffval.Node) = New_BinOp( DOUBLE , (ffvsp[-1].Node), ACCUM, New_Const( DOUBLE, &zero, sizeof(zero) ));
		  } else if ((FSTRCMP((ffvsp[-2].str),"SEQDIFF(") == 0) && (TYPE((ffvsp[-1].Node)) == LONG)) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG ,   (ffvsp[-1].Node), DIFF, New_Const( LONG,   &zero, sizeof(zero) ));
		  } else if ((FSTRCMP((ffvsp[-2].str),"SEQDIFF(") == 0) && (TYPE((ffvsp[-1].Node)) == DOUBLE)) {
		    double zero = 0;
		    (ffval.Node) = New_BinOp( DOUBLE , (ffvsp[-1].Node), DIFF, New_Const( DOUBLE, &zero, sizeof(zero) ));
		  } else if (FSTRCMP((ffvsp[-2].str),"ABS(") == 0)
		     (ffval.Node) = New_Func( 0, abs_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
 		  else if (FSTRCMP((ffvsp[-2].str),"MIN(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[-1].Node)),  /* Force 1D result */
				    min1_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[-2].str),"MAX(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[-1].Node)),  /* Force 1D result */
				    max1_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[-2].str),"RANDOM(") == 0) { /* Vector RANDOM() */
                     (ffval.Node) = New_Func( 0, rnd_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     TEST((ffval.Node));
		     TYPE((ffval.Node)) = DOUBLE;
		  } else if (FSTRCMP((ffvsp[-2].str),"RANDOMN(") == 0) {
		     (ffval.Node) = New_Func( 0, gasrnd_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
=======
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 563 "eval.y"
    { if (FSTRCMP((ffvsp[(1) - (3)].str),"SUM(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[(2) - (3)].Node)), sum_fct, 1, (ffvsp[(2) - (3)].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"AVERAGE(") == 0)
		     (ffval.Node) = New_Func( DOUBLE, average_fct, 1, (ffvsp[(2) - (3)].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"STDDEV(") == 0)
		     (ffval.Node) = New_Func( DOUBLE, stddev_fct, 1, (ffvsp[(2) - (3)].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"MEDIAN(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[(2) - (3)].Node)), median_fct, 1, (ffvsp[(2) - (3)].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"NELEM(") == 0)
                     (ffval.Node) = New_Const( LONG, &( SIZE((ffvsp[(2) - (3)].Node)) ), sizeof(long) );
		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"NVALID(") == 0)
		     (ffval.Node) = New_Func( LONG, nonnull_fct, 1, (ffvsp[(2) - (3)].Node),
				    0, 0, 0, 0, 0, 0 );
		  else if   ((FSTRCMP((ffvsp[(1) - (3)].str),"ACCUM(") == 0) && (TYPE((ffvsp[(2) - (3)].Node)) == LONG)) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG ,   (ffvsp[(2) - (3)].Node), ACCUM, New_Const( LONG,   &zero, sizeof(zero) ));
		  } else if ((FSTRCMP((ffvsp[(1) - (3)].str),"ACCUM(") == 0) && (TYPE((ffvsp[(2) - (3)].Node)) == DOUBLE)) {
		    double zero = 0;
		    (ffval.Node) = New_BinOp( DOUBLE , (ffvsp[(2) - (3)].Node), ACCUM, New_Const( DOUBLE, &zero, sizeof(zero) ));
		  } else if ((FSTRCMP((ffvsp[(1) - (3)].str),"SEQDIFF(") == 0) && (TYPE((ffvsp[(2) - (3)].Node)) == LONG)) {
		    long zero = 0;
		    (ffval.Node) = New_BinOp( LONG ,   (ffvsp[(2) - (3)].Node), DIFF, New_Const( LONG,   &zero, sizeof(zero) ));
		  } else if ((FSTRCMP((ffvsp[(1) - (3)].str),"SEQDIFF(") == 0) && (TYPE((ffvsp[(2) - (3)].Node)) == DOUBLE)) {
		    double zero = 0;
		    (ffval.Node) = New_BinOp( DOUBLE , (ffvsp[(2) - (3)].Node), DIFF, New_Const( DOUBLE, &zero, sizeof(zero) ));
		  } else if (FSTRCMP((ffvsp[(1) - (3)].str),"ABS(") == 0)
		     (ffval.Node) = New_Func( 0, abs_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
 		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"MIN(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[(2) - (3)].Node)),  /* Force 1D result */
				    min1_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"MAX(") == 0)
		     (ffval.Node) = New_Func( TYPE((ffvsp[(2) - (3)].Node)),  /* Force 1D result */
				    max1_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		  else if (FSTRCMP((ffvsp[(1) - (3)].str),"RANDOM(") == 0) { /* Vector RANDOM() */
                     (ffval.Node) = New_Func( 0, rnd_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     TEST((ffval.Node));
		     TYPE((ffval.Node)) = DOUBLE;
		  } else if (FSTRCMP((ffvsp[(1) - (3)].str),"RANDOMN(") == 0) {
		     (ffval.Node) = New_Func( 0, gasrnd_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     TEST((ffval.Node));
		     TYPE((ffval.Node)) = DOUBLE;
                  } 
  		  else {  /*  These all take DOUBLE arguments  */
<<<<<<< HEAD
		     if( TYPE((ffvsp[-1].Node)) != DOUBLE ) (ffvsp[-1].Node) = New_Unary( DOUBLE, 0, (ffvsp[-1].Node) );
                     if (FSTRCMP((ffvsp[-2].str),"SIN(") == 0)
			(ffval.Node) = New_Func( 0, sin_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"COS(") == 0)
			(ffval.Node) = New_Func( 0, cos_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"TAN(") == 0)
			(ffval.Node) = New_Func( 0, tan_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"ARCSIN(") == 0
			      || FSTRCMP((ffvsp[-2].str),"ASIN(") == 0)
			(ffval.Node) = New_Func( 0, asin_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"ARCCOS(") == 0
			      || FSTRCMP((ffvsp[-2].str),"ACOS(") == 0)
			(ffval.Node) = New_Func( 0, acos_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"ARCTAN(") == 0
			      || FSTRCMP((ffvsp[-2].str),"ATAN(") == 0)
			(ffval.Node) = New_Func( 0, atan_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"SINH(") == 0)
			(ffval.Node) = New_Func( 0, sinh_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"COSH(") == 0)
			(ffval.Node) = New_Func( 0, cosh_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"TANH(") == 0)
			(ffval.Node) = New_Func( 0, tanh_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"EXP(") == 0)
			(ffval.Node) = New_Func( 0, exp_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"LOG(") == 0)
			(ffval.Node) = New_Func( 0, log_fct,  1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"LOG10(") == 0)
			(ffval.Node) = New_Func( 0, log10_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"SQRT(") == 0)
			(ffval.Node) = New_Func( 0, sqrt_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"ROUND(") == 0)
			(ffval.Node) = New_Func( 0, round_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"FLOOR(") == 0)
			(ffval.Node) = New_Func( 0, floor_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"CEIL(") == 0)
			(ffval.Node) = New_Func( 0, ceil_fct, 1, (ffvsp[-1].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[-2].str),"RANDOMP(") == 0) {
		       (ffval.Node) = New_Func( 0, poirnd_fct, 1, (ffvsp[-1].Node), 
=======
		     if( TYPE((ffvsp[(2) - (3)].Node)) != DOUBLE ) (ffvsp[(2) - (3)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(2) - (3)].Node) );
                     if (FSTRCMP((ffvsp[(1) - (3)].str),"SIN(") == 0)
			(ffval.Node) = New_Func( 0, sin_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"COS(") == 0)
			(ffval.Node) = New_Func( 0, cos_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"TAN(") == 0)
			(ffval.Node) = New_Func( 0, tan_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"ARCSIN(") == 0
			      || FSTRCMP((ffvsp[(1) - (3)].str),"ASIN(") == 0)
			(ffval.Node) = New_Func( 0, asin_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"ARCCOS(") == 0
			      || FSTRCMP((ffvsp[(1) - (3)].str),"ACOS(") == 0)
			(ffval.Node) = New_Func( 0, acos_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"ARCTAN(") == 0
			      || FSTRCMP((ffvsp[(1) - (3)].str),"ATAN(") == 0)
			(ffval.Node) = New_Func( 0, atan_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"SINH(") == 0)
			(ffval.Node) = New_Func( 0, sinh_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"COSH(") == 0)
			(ffval.Node) = New_Func( 0, cosh_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"TANH(") == 0)
			(ffval.Node) = New_Func( 0, tanh_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"EXP(") == 0)
			(ffval.Node) = New_Func( 0, exp_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"LOG(") == 0)
			(ffval.Node) = New_Func( 0, log_fct,  1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"LOG10(") == 0)
			(ffval.Node) = New_Func( 0, log10_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"SQRT(") == 0)
			(ffval.Node) = New_Func( 0, sqrt_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"ROUND(") == 0)
			(ffval.Node) = New_Func( 0, round_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"FLOOR(") == 0)
			(ffval.Node) = New_Func( 0, floor_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"CEIL(") == 0)
			(ffval.Node) = New_Func( 0, ceil_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0, 0, 0, 0, 0 );
		     else if (FSTRCMP((ffvsp[(1) - (3)].str),"RANDOMP(") == 0) {
		       (ffval.Node) = New_Func( 0, poirnd_fct, 1, (ffvsp[(2) - (3)].Node), 
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
				      0, 0, 0, 0, 0, 0 );
		       TYPE((ffval.Node)) = LONG;
		     } else {
			fferror("Function(expr) not supported");
			FFERROR;
		     }
		  }
                  TEST((ffval.Node)); 
                }
<<<<<<< HEAD
#line 2552 "y.tab.c"
    break;

  case 59: /* expr: IFUNCTION sexpr ',' sexpr ')'  */
#line 693 "eval.y"
                { 
		  if (FSTRCMP((ffvsp[-4].str),"STRSTR(") == 0) {
		    (ffval.Node) = New_Func( LONG, strpos_fct, 2, (ffvsp[-3].Node), (ffvsp[-1].Node), 0, 
=======
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 658 "eval.y"
    { 
		  if (FSTRCMP((ffvsp[(1) - (5)].str),"STRSTR(") == 0) {
		    (ffval.Node) = New_Func( LONG, strpos_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0, 
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
				   0, 0, 0, 0 );
		    TEST((ffval.Node));
		  }
                }
<<<<<<< HEAD
#line 2564 "y.tab.c"
    break;

  case 60: /* expr: FUNCTION expr ',' expr ')'  */
#line 701 "eval.y"
                { 
		   if (FSTRCMP((ffvsp[-4].str),"DEFNULL(") == 0) {
		      if( SIZE((ffvsp[-3].Node))>=SIZE((ffvsp[-1].Node)) && Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) ) ) {
			 PROMOTE((ffvsp[-3].Node),(ffvsp[-1].Node));
			 (ffval.Node) = New_Func( 0, defnull_fct, 2, (ffvsp[-3].Node), (ffvsp[-1].Node), 0,
=======
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 666 "eval.y"
    { 
		   if (FSTRCMP((ffvsp[(1) - (5)].str),"DEFNULL(") == 0) {
		      if( SIZE((ffvsp[(2) - (5)].Node))>=SIZE((ffvsp[(4) - (5)].Node)) && Test_Dims( (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node) ) ) {
			 PROMOTE((ffvsp[(2) - (5)].Node),(ffvsp[(4) - (5)].Node));
			 (ffval.Node) = New_Func( 0, defnull_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0,
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
					0, 0, 0, 0 );
			 TEST((ffval.Node)); 
		      } else {
			 fferror("Dimensions of DEFNULL arguments "
				 "are not compatible");
			 FFERROR;
		      }
<<<<<<< HEAD
		   } else if (FSTRCMP((ffvsp[-4].str),"ARCTAN2(") == 0) {
		     if( TYPE((ffvsp[-3].Node)) != DOUBLE ) (ffvsp[-3].Node) = New_Unary( DOUBLE, 0, (ffvsp[-3].Node) );
		     if( TYPE((ffvsp[-1].Node)) != DOUBLE ) (ffvsp[-1].Node) = New_Unary( DOUBLE, 0, (ffvsp[-1].Node) );
		     if( Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) ) ) {
			(ffval.Node) = New_Func( 0, atan2_fct, 2, (ffvsp[-3].Node), (ffvsp[-1].Node), 0, 0, 0, 0, 0 );
			TEST((ffval.Node)); 
			if( SIZE((ffvsp[-3].Node))<SIZE((ffvsp[-1].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-1].Node));
=======
		   } else if (FSTRCMP((ffvsp[(1) - (5)].str),"ARCTAN2(") == 0) {
		     if( TYPE((ffvsp[(2) - (5)].Node)) != DOUBLE ) (ffvsp[(2) - (5)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(2) - (5)].Node) );
		     if( TYPE((ffvsp[(4) - (5)].Node)) != DOUBLE ) (ffvsp[(4) - (5)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(4) - (5)].Node) );
		     if( Test_Dims( (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node) ) ) {
			(ffval.Node) = New_Func( 0, atan2_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0, 0, 0, 0, 0 );
			TEST((ffval.Node)); 
			if( SIZE((ffvsp[(2) - (5)].Node))<SIZE((ffvsp[(4) - (5)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(4) - (5)].Node));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     } else {
			fferror("Dimensions of arctan2 arguments "
				"are not compatible");
			FFERROR;
		     }
<<<<<<< HEAD
		   } else if (FSTRCMP((ffvsp[-4].str),"MIN(") == 0) {
		      PROMOTE( (ffvsp[-3].Node), (ffvsp[-1].Node) );
		      if( Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) ) ) {
			(ffval.Node) = New_Func( 0, min2_fct, 2, (ffvsp[-3].Node), (ffvsp[-1].Node), 0, 0, 0, 0, 0 );
			TEST((ffval.Node));
			if( SIZE((ffvsp[-3].Node))<SIZE((ffvsp[-1].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-1].Node));
=======
		   } else if (FSTRCMP((ffvsp[(1) - (5)].str),"MIN(") == 0) {
		      PROMOTE( (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node) );
		      if( Test_Dims( (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node) ) ) {
			(ffval.Node) = New_Func( 0, min2_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0, 0, 0, 0, 0 );
			TEST((ffval.Node));
			if( SIZE((ffvsp[(2) - (5)].Node))<SIZE((ffvsp[(4) - (5)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(4) - (5)].Node));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		      } else {
			fferror("Dimensions of min(a,b) arguments "
				"are not compatible");
			FFERROR;
		      }
<<<<<<< HEAD
		   } else if (FSTRCMP((ffvsp[-4].str),"MAX(") == 0) {
		      PROMOTE( (ffvsp[-3].Node), (ffvsp[-1].Node) );
		      if( Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) ) ) {
			(ffval.Node) = New_Func( 0, max2_fct, 2, (ffvsp[-3].Node), (ffvsp[-1].Node), 0, 0, 0, 0, 0 );
			TEST((ffval.Node));
			if( SIZE((ffvsp[-3].Node))<SIZE((ffvsp[-1].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-1].Node));
=======
		   } else if (FSTRCMP((ffvsp[(1) - (5)].str),"MAX(") == 0) {
		      PROMOTE( (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node) );
		      if( Test_Dims( (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node) ) ) {
			(ffval.Node) = New_Func( 0, max2_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0, 0, 0, 0, 0 );
			TEST((ffval.Node));
			if( SIZE((ffvsp[(2) - (5)].Node))<SIZE((ffvsp[(4) - (5)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(4) - (5)].Node));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		      } else {
			fferror("Dimensions of max(a,b) arguments "
				"are not compatible");
			FFERROR;
		      }
<<<<<<< HEAD
		   } else if (FSTRCMP((ffvsp[-4].str),"SETNULL(") == 0) {
		     if (OPER((ffvsp[-3].Node)) != CONST_OP
			 || SIZE((ffvsp[-3].Node)) != 1) {
		       fferror("SETNULL first argument must be a scalar constant");
		       FFERROR;
		     }
		     /* Make sure first arg is same type as second arg */
		     if ( TYPE((ffvsp[-3].Node)) != TYPE((ffvsp[-1].Node)) ) (ffvsp[-3].Node) = New_Unary( TYPE((ffvsp[-1].Node)), 0, (ffvsp[-3].Node) );
		     (ffval.Node) = New_Func( 0, setnull_fct, 2, (ffvsp[-1].Node), (ffvsp[-3].Node), 0, 0, 0, 0, 0 );
=======
#if 0
		   } else if (FSTRCMP((ffvsp[(1) - (5)].str),"STRSTR(") == 0) {
		     if( TYPE((ffvsp[(2) - (5)].Node)) != STRING || TYPE((ffvsp[(4) - (5)].Node)) != STRING) {
		       fferror("Arguments to strstr(s,r) must be strings");
		       FFERROR;
		     }
		     (ffval.Node) = New_Func( LONG, strpos_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0, 
				    0, 0, 0, 0 );
		     TEST((ffval.Node));
#endif
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		   } else {
		      fferror("Function(expr,expr) not supported");
		      FFERROR;
		   }
                }
<<<<<<< HEAD
#line 2629 "y.tab.c"
    break;

  case 61: /* expr: FUNCTION expr ',' expr ',' expr ',' expr ')'  */
#line 762 "eval.y"
                { 
		  if (FSTRCMP((ffvsp[-8].str),"ANGSEP(") == 0) {
		    if( TYPE((ffvsp[-7].Node)) != DOUBLE ) (ffvsp[-7].Node) = New_Unary( DOUBLE, 0, (ffvsp[-7].Node) );
		    if( TYPE((ffvsp[-5].Node)) != DOUBLE ) (ffvsp[-5].Node) = New_Unary( DOUBLE, 0, (ffvsp[-5].Node) );
		    if( TYPE((ffvsp[-3].Node)) != DOUBLE ) (ffvsp[-3].Node) = New_Unary( DOUBLE, 0, (ffvsp[-3].Node) );
		    if( TYPE((ffvsp[-1].Node)) != DOUBLE ) (ffvsp[-1].Node) = New_Unary( DOUBLE, 0, (ffvsp[-1].Node) );
		    if( Test_Dims( (ffvsp[-7].Node), (ffvsp[-5].Node) ) && Test_Dims( (ffvsp[-5].Node), (ffvsp[-3].Node) ) && 
			Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) ) ) {
		      (ffval.Node) = New_Func( 0, angsep_fct, 4, (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),0,0,0 );
		      TEST((ffval.Node)); 
		      if( SIZE((ffvsp[-7].Node))<SIZE((ffvsp[-5].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-5].Node));
		      if( SIZE((ffvsp[-5].Node))<SIZE((ffvsp[-3].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-3].Node));
		      if( SIZE((ffvsp[-3].Node))<SIZE((ffvsp[-1].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-1].Node));
=======
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 728 "eval.y"
    { 
		  if (FSTRCMP((ffvsp[(1) - (9)].str),"ANGSEP(") == 0) {
		    if( TYPE((ffvsp[(2) - (9)].Node)) != DOUBLE ) (ffvsp[(2) - (9)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(2) - (9)].Node) );
		    if( TYPE((ffvsp[(4) - (9)].Node)) != DOUBLE ) (ffvsp[(4) - (9)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(4) - (9)].Node) );
		    if( TYPE((ffvsp[(6) - (9)].Node)) != DOUBLE ) (ffvsp[(6) - (9)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(6) - (9)].Node) );
		    if( TYPE((ffvsp[(8) - (9)].Node)) != DOUBLE ) (ffvsp[(8) - (9)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(8) - (9)].Node) );
		    if( Test_Dims( (ffvsp[(2) - (9)].Node), (ffvsp[(4) - (9)].Node) ) && Test_Dims( (ffvsp[(4) - (9)].Node), (ffvsp[(6) - (9)].Node) ) && 
			Test_Dims( (ffvsp[(6) - (9)].Node), (ffvsp[(8) - (9)].Node) ) ) {
		      (ffval.Node) = New_Func( 0, angsep_fct, 4, (ffvsp[(2) - (9)].Node), (ffvsp[(4) - (9)].Node), (ffvsp[(6) - (9)].Node), (ffvsp[(8) - (9)].Node),0,0,0 );
		      TEST((ffval.Node)); 
		      if( SIZE((ffvsp[(2) - (9)].Node))<SIZE((ffvsp[(4) - (9)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(4) - (9)].Node));
		      if( SIZE((ffvsp[(4) - (9)].Node))<SIZE((ffvsp[(6) - (9)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(6) - (9)].Node));
		      if( SIZE((ffvsp[(6) - (9)].Node))<SIZE((ffvsp[(8) - (9)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(8) - (9)].Node));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		    } else {
		      fferror("Dimensions of ANGSEP arguments "
			      "are not compatible");
		      FFERROR;
		    }
		   } else {
		      fferror("Function(expr,expr,expr,expr) not supported");
		      FFERROR;
		   }
                }
<<<<<<< HEAD
#line 2657 "y.tab.c"
    break;

  case 62: /* expr: expr '[' expr ']'  */
#line 786 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-3].Node), 1, (ffvsp[-1].Node),  0,  0,  0,   0 ); TEST((ffval.Node)); }
#line 2663 "y.tab.c"
    break;

  case 63: /* expr: expr '[' expr ',' expr ']'  */
#line 788 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-5].Node), 2, (ffvsp[-3].Node), (ffvsp[-1].Node),  0,  0,   0 ); TEST((ffval.Node)); }
#line 2669 "y.tab.c"
    break;

  case 64: /* expr: expr '[' expr ',' expr ',' expr ']'  */
#line 790 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-7].Node), 3, (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),  0,   0 ); TEST((ffval.Node)); }
#line 2675 "y.tab.c"
    break;

  case 65: /* expr: expr '[' expr ',' expr ',' expr ',' expr ']'  */
#line 792 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-9].Node), 4, (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),   0 ); TEST((ffval.Node)); }
#line 2681 "y.tab.c"
    break;

  case 66: /* expr: expr '[' expr ',' expr ',' expr ',' expr ',' expr ']'  */
#line 794 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-11].Node), 5, (ffvsp[-9].Node), (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node) ); TEST((ffval.Node)); }
#line 2687 "y.tab.c"
    break;

  case 67: /* expr: INTCAST expr  */
#line 796 "eval.y"
                { (ffval.Node) = New_Unary( LONG,   INTCAST, (ffvsp[0].Node) );  TEST((ffval.Node));  }
#line 2693 "y.tab.c"
    break;

  case 68: /* expr: INTCAST bexpr  */
#line 798 "eval.y"
                { (ffval.Node) = New_Unary( LONG,   INTCAST, (ffvsp[0].Node) );  TEST((ffval.Node));  }
#line 2699 "y.tab.c"
    break;

  case 69: /* expr: FLTCAST expr  */
#line 800 "eval.y"
                { (ffval.Node) = New_Unary( DOUBLE, FLTCAST, (ffvsp[0].Node) );  TEST((ffval.Node));  }
#line 2705 "y.tab.c"
    break;

  case 70: /* expr: FLTCAST bexpr  */
#line 802 "eval.y"
                { (ffval.Node) = New_Unary( DOUBLE, FLTCAST, (ffvsp[0].Node) );  TEST((ffval.Node));  }
#line 2711 "y.tab.c"
    break;

  case 71: /* bexpr: BOOLEAN  */
#line 806 "eval.y"
                { (ffval.Node) = New_Const( BOOLEAN, &((ffvsp[0].log)), sizeof(char) ); TEST((ffval.Node)); }
#line 2717 "y.tab.c"
    break;

  case 72: /* bexpr: BCOLUMN  */
#line 808 "eval.y"
                { (ffval.Node) = New_Column( (ffvsp[0].lng) ); TEST((ffval.Node)); }
#line 2723 "y.tab.c"
    break;

  case 73: /* bexpr: BCOLUMN '{' expr '}'  */
#line 810 "eval.y"
                {
                  if( TYPE((ffvsp[-1].Node)) != LONG
		      || OPER((ffvsp[-1].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[-3].lng), (ffvsp[-1].Node) ); TEST((ffval.Node));
                }
#line 2736 "y.tab.c"
    break;

  case 74: /* bexpr: bits EQ bits  */
#line 819 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), EQ,  (ffvsp[0].Node) ); TEST((ffval.Node));
		  SIZE((ffval.Node)) = 1;                                     }
#line 2743 "y.tab.c"
    break;

  case 75: /* bexpr: bits NE bits  */
#line 822 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), NE,  (ffvsp[0].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
#line 2750 "y.tab.c"
    break;

  case 76: /* bexpr: bits LT bits  */
#line 825 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), LT,  (ffvsp[0].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
#line 2757 "y.tab.c"
    break;

  case 77: /* bexpr: bits LTE bits  */
#line 828 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), LTE, (ffvsp[0].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
#line 2764 "y.tab.c"
    break;

  case 78: /* bexpr: bits GT bits  */
#line 831 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), GT,  (ffvsp[0].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
#line 2771 "y.tab.c"
    break;

  case 79: /* bexpr: bits GTE bits  */
#line 834 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), GTE, (ffvsp[0].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
#line 2778 "y.tab.c"
    break;

  case 80: /* bexpr: expr GT expr  */
#line 837 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), GT,  (ffvsp[0].Node) );
                  TEST((ffval.Node));                                               }
#line 2785 "y.tab.c"
    break;

  case 81: /* bexpr: expr LT expr  */
#line 840 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), LT,  (ffvsp[0].Node) );
                  TEST((ffval.Node));                                               }
#line 2792 "y.tab.c"
    break;

  case 82: /* bexpr: expr GTE expr  */
#line 843 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), GTE, (ffvsp[0].Node) );
                  TEST((ffval.Node));                                               }
#line 2799 "y.tab.c"
    break;

  case 83: /* bexpr: expr LTE expr  */
#line 846 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), LTE, (ffvsp[0].Node) );
                  TEST((ffval.Node));                                               }
#line 2806 "y.tab.c"
    break;

  case 84: /* bexpr: expr '~' expr  */
#line 849 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), '~', (ffvsp[0].Node) );
                  TEST((ffval.Node));                                               }
#line 2813 "y.tab.c"
    break;

  case 85: /* bexpr: expr EQ expr  */
#line 852 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), EQ,  (ffvsp[0].Node) );
                  TEST((ffval.Node));                                               }
#line 2820 "y.tab.c"
    break;

  case 86: /* bexpr: expr NE expr  */
#line 855 "eval.y"
                { PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), NE,  (ffvsp[0].Node) );
                  TEST((ffval.Node));                                               }
#line 2827 "y.tab.c"
    break;

  case 87: /* bexpr: sexpr EQ sexpr  */
#line 858 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), EQ,  (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
#line 2834 "y.tab.c"
    break;

  case 88: /* bexpr: sexpr NE sexpr  */
#line 861 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), NE,  (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
#line 2841 "y.tab.c"
    break;

  case 89: /* bexpr: sexpr GT sexpr  */
#line 864 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), GT,  (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
#line 2848 "y.tab.c"
    break;

  case 90: /* bexpr: sexpr GTE sexpr  */
#line 867 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), GTE, (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
#line 2855 "y.tab.c"
    break;

  case 91: /* bexpr: sexpr LT sexpr  */
#line 870 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), LT,  (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
#line 2862 "y.tab.c"
    break;

  case 92: /* bexpr: sexpr LTE sexpr  */
#line 873 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), LTE, (ffvsp[0].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
#line 2869 "y.tab.c"
    break;

  case 93: /* bexpr: bexpr AND bexpr  */
#line 876 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), AND, (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 2875 "y.tab.c"
    break;

  case 94: /* bexpr: bexpr OR bexpr  */
#line 878 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), OR,  (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 2881 "y.tab.c"
    break;

  case 95: /* bexpr: bexpr EQ bexpr  */
#line 880 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), EQ,  (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 2887 "y.tab.c"
    break;

  case 96: /* bexpr: bexpr NE bexpr  */
#line 882 "eval.y"
                { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), NE,  (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 2893 "y.tab.c"
    break;

  case 97: /* bexpr: expr '=' expr ':' expr  */
#line 885 "eval.y"
                { PROMOTE((ffvsp[-4].Node),(ffvsp[-2].Node)); PROMOTE((ffvsp[-4].Node),(ffvsp[0].Node)); PROMOTE((ffvsp[-2].Node),(ffvsp[0].Node));
		  (ffvsp[-2].Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), LTE, (ffvsp[-4].Node) );
                  (ffvsp[0].Node) = New_BinOp( BOOLEAN, (ffvsp[-4].Node), LTE, (ffvsp[0].Node) );
                  (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[-2].Node), AND, (ffvsp[0].Node) );
                  TEST((ffval.Node));                                         }
#line 2903 "y.tab.c"
    break;

  case 98: /* bexpr: bexpr '?' bexpr ':' bexpr  */
#line 892 "eval.y"
                {
                  if( ! Test_Dims((ffvsp[-2].Node),(ffvsp[0].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[-2].Node), (ffvsp[0].Node), (ffvsp[-4].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[-2].Node))<SIZE((ffvsp[0].Node)) )  Copy_Dims((ffval.Node), (ffvsp[0].Node));
                  if( ! Test_Dims((ffvsp[-4].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  if( SIZE((ffval.Node))<SIZE((ffvsp[-4].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-4].Node));
                }
#line 2923 "y.tab.c"
    break;

  case 99: /* bexpr: BFUNCTION expr ')'  */
#line 909 "eval.y"
                {
		   if (FSTRCMP((ffvsp[-2].str),"ISNULL(") == 0) {
		      (ffval.Node) = New_Func( 0, isnull_fct, 1, (ffvsp[-1].Node), 0, 0,
=======
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 752 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (4)].Node), 1, (ffvsp[(3) - (4)].Node),  0,  0,  0,   0 ); TEST((ffval.Node)); }
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 754 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (6)].Node), 2, (ffvsp[(3) - (6)].Node), (ffvsp[(5) - (6)].Node),  0,  0,   0 ); TEST((ffval.Node)); }
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 756 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (8)].Node), 3, (ffvsp[(3) - (8)].Node), (ffvsp[(5) - (8)].Node), (ffvsp[(7) - (8)].Node),  0,   0 ); TEST((ffval.Node)); }
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 758 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (10)].Node), 4, (ffvsp[(3) - (10)].Node), (ffvsp[(5) - (10)].Node), (ffvsp[(7) - (10)].Node), (ffvsp[(9) - (10)].Node),   0 ); TEST((ffval.Node)); }
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 760 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (12)].Node), 5, (ffvsp[(3) - (12)].Node), (ffvsp[(5) - (12)].Node), (ffvsp[(7) - (12)].Node), (ffvsp[(9) - (12)].Node), (ffvsp[(11) - (12)].Node) ); TEST((ffval.Node)); }
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 762 "eval.y"
    { (ffval.Node) = New_Unary( LONG,   INTCAST, (ffvsp[(2) - (2)].Node) );  TEST((ffval.Node));  }
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 764 "eval.y"
    { (ffval.Node) = New_Unary( LONG,   INTCAST, (ffvsp[(2) - (2)].Node) );  TEST((ffval.Node));  }
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 766 "eval.y"
    { (ffval.Node) = New_Unary( DOUBLE, FLTCAST, (ffvsp[(2) - (2)].Node) );  TEST((ffval.Node));  }
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 768 "eval.y"
    { (ffval.Node) = New_Unary( DOUBLE, FLTCAST, (ffvsp[(2) - (2)].Node) );  TEST((ffval.Node));  }
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 772 "eval.y"
    { (ffval.Node) = New_Const( BOOLEAN, &((ffvsp[(1) - (1)].log)), sizeof(char) ); TEST((ffval.Node)); }
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 774 "eval.y"
    { (ffval.Node) = New_Column( (ffvsp[(1) - (1)].lng) ); TEST((ffval.Node)); }
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 776 "eval.y"
    {
                  if( TYPE((ffvsp[(3) - (4)].Node)) != LONG
		      || OPER((ffvsp[(3) - (4)].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[(1) - (4)].lng), (ffvsp[(3) - (4)].Node) ); TEST((ffval.Node));
                }
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 785 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), EQ,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
		  SIZE((ffval.Node)) = 1;                                     }
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 788 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), NE,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 791 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), LT,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 794 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), LTE, (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 797 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), GT,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 800 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), GTE, (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); 
		  SIZE((ffval.Node)) = 1;                                     }
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 803 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), GT,  (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                               }
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 806 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), LT,  (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                               }
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 809 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), GTE, (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                               }
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 812 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), LTE, (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                               }
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 815 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), '~', (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                               }
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 818 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), EQ,  (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                               }
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 821 "eval.y"
    { PROMOTE((ffvsp[(1) - (3)].Node),(ffvsp[(3) - (3)].Node)); (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), NE,  (ffvsp[(3) - (3)].Node) );
                  TEST((ffval.Node));                                               }
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 824 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), EQ,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 827 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), NE,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 830 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), GT,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 833 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), GTE, (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 836 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), LT,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 839 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), LTE, (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = 1; }
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 842 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), AND, (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); }
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 844 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), OR,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); }
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 846 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), EQ,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); }
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 848 "eval.y"
    { (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (3)].Node), NE,  (ffvsp[(3) - (3)].Node) ); TEST((ffval.Node)); }
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 851 "eval.y"
    { PROMOTE((ffvsp[(1) - (5)].Node),(ffvsp[(3) - (5)].Node)); PROMOTE((ffvsp[(1) - (5)].Node),(ffvsp[(5) - (5)].Node)); PROMOTE((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node));
		  (ffvsp[(3) - (5)].Node) = New_BinOp( BOOLEAN, (ffvsp[(3) - (5)].Node), LTE, (ffvsp[(1) - (5)].Node) );
                  (ffvsp[(5) - (5)].Node) = New_BinOp( BOOLEAN, (ffvsp[(1) - (5)].Node), LTE, (ffvsp[(5) - (5)].Node) );
                  (ffval.Node) = New_BinOp( BOOLEAN, (ffvsp[(3) - (5)].Node), AND, (ffvsp[(5) - (5)].Node) );
                  TEST((ffval.Node));                                         }
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 858 "eval.y"
    {
                  if( ! Test_Dims((ffvsp[(3) - (5)].Node),(ffvsp[(5) - (5)].Node)) ) {
                     fferror("Incompatible dimensions in '?:' arguments");
		     FFERROR;
                  }
                  (ffval.Node) = New_Func( 0, ifthenelse_fct, 3, (ffvsp[(3) - (5)].Node), (ffvsp[(5) - (5)].Node), (ffvsp[(1) - (5)].Node),
                                 0, 0, 0, 0 );
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[(3) - (5)].Node))<SIZE((ffvsp[(5) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(5) - (5)].Node));
                  if( ! Test_Dims((ffvsp[(1) - (5)].Node),(ffval.Node)) ) {
                     fferror("Incompatible dimensions in '?:' condition");
		     FFERROR;
                  }
                  if( SIZE((ffval.Node))<SIZE((ffvsp[(1) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(1) - (5)].Node));
                }
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 875 "eval.y"
    {
		   if (FSTRCMP((ffvsp[(1) - (3)].str),"ISNULL(") == 0) {
		      (ffval.Node) = New_Func( 0, isnull_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0,
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
				     0, 0, 0, 0 );
		      TEST((ffval.Node)); 
                      /* Use expression's size, but return BOOLEAN */
		      TYPE((ffval.Node)) = BOOLEAN;
		   } else {
		      fferror("Boolean Function(expr) not supported");
		      FFERROR;
		   }
		}
<<<<<<< HEAD
#line 2940 "y.tab.c"
    break;

  case 100: /* bexpr: BFUNCTION bexpr ')'  */
#line 922 "eval.y"
                {
		   if (FSTRCMP((ffvsp[-2].str),"ISNULL(") == 0) {
		      (ffval.Node) = New_Func( 0, isnull_fct, 1, (ffvsp[-1].Node), 0, 0,
=======
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 888 "eval.y"
    {
		   if (FSTRCMP((ffvsp[(1) - (3)].str),"ISNULL(") == 0) {
		      (ffval.Node) = New_Func( 0, isnull_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0,
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
				     0, 0, 0, 0 );
		      TEST((ffval.Node)); 
                      /* Use expression's size, but return BOOLEAN */
		      TYPE((ffval.Node)) = BOOLEAN;
		   } else {
		      fferror("Boolean Function(expr) not supported");
		      FFERROR;
		   }
		}
<<<<<<< HEAD
#line 2957 "y.tab.c"
    break;

  case 101: /* bexpr: BFUNCTION sexpr ')'  */
#line 935 "eval.y"
                {
		   if (FSTRCMP((ffvsp[-2].str),"ISNULL(") == 0) {
		      (ffval.Node) = New_Func( BOOLEAN, isnull_fct, 1, (ffvsp[-1].Node), 0, 0,
=======
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 901 "eval.y"
    {
		   if (FSTRCMP((ffvsp[(1) - (3)].str),"ISNULL(") == 0) {
		      (ffval.Node) = New_Func( BOOLEAN, isnull_fct, 1, (ffvsp[(2) - (3)].Node), 0, 0,
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
				     0, 0, 0, 0 );
		      TEST((ffval.Node)); 
		   } else {
		      fferror("Boolean Function(expr) not supported");
		      FFERROR;
		   }
		}
<<<<<<< HEAD
#line 2972 "y.tab.c"
    break;

  case 102: /* bexpr: FUNCTION bexpr ',' bexpr ')'  */
#line 946 "eval.y"
                {
		   if (FSTRCMP((ffvsp[-4].str),"DEFNULL(") == 0) {
		      if( SIZE((ffvsp[-3].Node))>=SIZE((ffvsp[-1].Node)) && Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) ) ) {
			 (ffval.Node) = New_Func( 0, defnull_fct, 2, (ffvsp[-3].Node), (ffvsp[-1].Node), 0,
=======
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 912 "eval.y"
    {
		   if (FSTRCMP((ffvsp[(1) - (5)].str),"DEFNULL(") == 0) {
		      if( SIZE((ffvsp[(2) - (5)].Node))>=SIZE((ffvsp[(4) - (5)].Node)) && Test_Dims( (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node) ) ) {
			 (ffval.Node) = New_Func( 0, defnull_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0,
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
					0, 0, 0, 0 );
			 TEST((ffval.Node)); 
		      } else {
			 fferror("Dimensions of DEFNULL arguments are not compatible");
			 FFERROR;
		      }
		   } else {
		      fferror("Boolean Function(expr,expr) not supported");
		      FFERROR;
		   }
		}
<<<<<<< HEAD
#line 2992 "y.tab.c"
    break;

  case 103: /* bexpr: BFUNCTION expr ',' expr ',' expr ')'  */
#line 962 "eval.y"
                {
		   if( TYPE((ffvsp[-5].Node)) != DOUBLE ) (ffvsp[-5].Node) = New_Unary( DOUBLE, 0, (ffvsp[-5].Node) );
		   if( TYPE((ffvsp[-3].Node)) != DOUBLE ) (ffvsp[-3].Node) = New_Unary( DOUBLE, 0, (ffvsp[-3].Node) );
		   if( TYPE((ffvsp[-1].Node)) != DOUBLE ) (ffvsp[-1].Node) = New_Unary( DOUBLE, 0, (ffvsp[-1].Node) );
		   if( ! (Test_Dims( (ffvsp[-5].Node), (ffvsp[-3].Node) ) && Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) ) ) ) {
=======
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 928 "eval.y"
    {
		   if( TYPE((ffvsp[(2) - (7)].Node)) != DOUBLE ) (ffvsp[(2) - (7)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(2) - (7)].Node) );
		   if( TYPE((ffvsp[(4) - (7)].Node)) != DOUBLE ) (ffvsp[(4) - (7)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(4) - (7)].Node) );
		   if( TYPE((ffvsp[(6) - (7)].Node)) != DOUBLE ) (ffvsp[(6) - (7)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(6) - (7)].Node) );
		   if( ! (Test_Dims( (ffvsp[(2) - (7)].Node), (ffvsp[(4) - (7)].Node) ) && Test_Dims( (ffvsp[(4) - (7)].Node), (ffvsp[(6) - (7)].Node) ) ) ) {
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		       fferror("Dimensions of NEAR arguments "
			       "are not compatible");
		       FFERROR;
		   } else {
<<<<<<< HEAD
		     if (FSTRCMP((ffvsp[-6].str),"NEAR(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, near_fct, 3, (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),
=======
		     if (FSTRCMP((ffvsp[(1) - (7)].str),"NEAR(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, near_fct, 3, (ffvsp[(2) - (7)].Node), (ffvsp[(4) - (7)].Node), (ffvsp[(6) - (7)].Node),
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
				      0, 0, 0, 0 );
		     } else {
		       fferror("Boolean Function not supported");
		       FFERROR;
		     }
		     TEST((ffval.Node)); 

<<<<<<< HEAD
		     if( SIZE((ffval.Node))<SIZE((ffvsp[-5].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-5].Node));
		     if( SIZE((ffvsp[-5].Node))<SIZE((ffvsp[-3].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-3].Node));
		     if( SIZE((ffvsp[-3].Node))<SIZE((ffvsp[-1].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-1].Node));
		   }
		}
#line 3020 "y.tab.c"
    break;

  case 104: /* bexpr: BFUNCTION expr ',' expr ',' expr ',' expr ',' expr ')'  */
#line 986 "eval.y"
                {
		   if( TYPE((ffvsp[-9].Node)) != DOUBLE ) (ffvsp[-9].Node) = New_Unary( DOUBLE, 0, (ffvsp[-9].Node) );
		   if( TYPE((ffvsp[-7].Node)) != DOUBLE ) (ffvsp[-7].Node) = New_Unary( DOUBLE, 0, (ffvsp[-7].Node) );
		   if( TYPE((ffvsp[-5].Node)) != DOUBLE ) (ffvsp[-5].Node) = New_Unary( DOUBLE, 0, (ffvsp[-5].Node) );
		   if( TYPE((ffvsp[-3].Node)) != DOUBLE ) (ffvsp[-3].Node) = New_Unary( DOUBLE, 0, (ffvsp[-3].Node) );
		   if( TYPE((ffvsp[-1].Node))!= DOUBLE ) (ffvsp[-1].Node)= New_Unary( DOUBLE, 0, (ffvsp[-1].Node));
		   if( ! (Test_Dims( (ffvsp[-9].Node), (ffvsp[-7].Node) ) && Test_Dims( (ffvsp[-7].Node), (ffvsp[-5].Node) ) && 
			  Test_Dims( (ffvsp[-5].Node), (ffvsp[-3].Node) ) && Test_Dims( (ffvsp[-3].Node), (ffvsp[-1].Node) )) ) {
=======
		     if( SIZE((ffval.Node))<SIZE((ffvsp[(2) - (7)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(2) - (7)].Node));
		     if( SIZE((ffvsp[(2) - (7)].Node))<SIZE((ffvsp[(4) - (7)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(4) - (7)].Node));
		     if( SIZE((ffvsp[(4) - (7)].Node))<SIZE((ffvsp[(6) - (7)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(6) - (7)].Node));
		   }
		}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 952 "eval.y"
    {
		   if( TYPE((ffvsp[(2) - (11)].Node)) != DOUBLE ) (ffvsp[(2) - (11)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(2) - (11)].Node) );
		   if( TYPE((ffvsp[(4) - (11)].Node)) != DOUBLE ) (ffvsp[(4) - (11)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(4) - (11)].Node) );
		   if( TYPE((ffvsp[(6) - (11)].Node)) != DOUBLE ) (ffvsp[(6) - (11)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(6) - (11)].Node) );
		   if( TYPE((ffvsp[(8) - (11)].Node)) != DOUBLE ) (ffvsp[(8) - (11)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(8) - (11)].Node) );
		   if( TYPE((ffvsp[(10) - (11)].Node))!= DOUBLE ) (ffvsp[(10) - (11)].Node)= New_Unary( DOUBLE, 0, (ffvsp[(10) - (11)].Node));
		   if( ! (Test_Dims( (ffvsp[(2) - (11)].Node), (ffvsp[(4) - (11)].Node) ) && Test_Dims( (ffvsp[(4) - (11)].Node), (ffvsp[(6) - (11)].Node) ) && 
			  Test_Dims( (ffvsp[(6) - (11)].Node), (ffvsp[(8) - (11)].Node) ) && Test_Dims( (ffvsp[(8) - (11)].Node), (ffvsp[(10) - (11)].Node) )) ) {
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     fferror("Dimensions of CIRCLE arguments "
			     "are not compatible");
		     FFERROR;
		   } else {
<<<<<<< HEAD
		     if (FSTRCMP((ffvsp[-10].str),"CIRCLE(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, circle_fct, 5, (ffvsp[-9].Node), (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node),
				      (ffvsp[-1].Node), 0, 0 );
=======
		     if (FSTRCMP((ffvsp[(1) - (11)].str),"CIRCLE(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, circle_fct, 5, (ffvsp[(2) - (11)].Node), (ffvsp[(4) - (11)].Node), (ffvsp[(6) - (11)].Node), (ffvsp[(8) - (11)].Node),
				      (ffvsp[(10) - (11)].Node), 0, 0 );
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     } else {
		       fferror("Boolean Function not supported");
		       FFERROR;
		     }
		     TEST((ffval.Node)); 
<<<<<<< HEAD
		     if( SIZE((ffval.Node))<SIZE((ffvsp[-9].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-9].Node));
		     if( SIZE((ffvsp[-9].Node))<SIZE((ffvsp[-7].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-7].Node));
		     if( SIZE((ffvsp[-7].Node))<SIZE((ffvsp[-5].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-5].Node));
		     if( SIZE((ffvsp[-5].Node))<SIZE((ffvsp[-3].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-3].Node));
		     if( SIZE((ffvsp[-3].Node))<SIZE((ffvsp[-1].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-1].Node));
		   }
		}
#line 3052 "y.tab.c"
    break;

  case 105: /* bexpr: BFUNCTION expr ',' expr ',' expr ',' expr ',' expr ',' expr ',' expr ')'  */
#line 1014 "eval.y"
                {
		   if( TYPE((ffvsp[-13].Node)) != DOUBLE ) (ffvsp[-13].Node) = New_Unary( DOUBLE, 0, (ffvsp[-13].Node) );
		   if( TYPE((ffvsp[-11].Node)) != DOUBLE ) (ffvsp[-11].Node) = New_Unary( DOUBLE, 0, (ffvsp[-11].Node) );
		   if( TYPE((ffvsp[-9].Node)) != DOUBLE ) (ffvsp[-9].Node) = New_Unary( DOUBLE, 0, (ffvsp[-9].Node) );
		   if( TYPE((ffvsp[-7].Node)) != DOUBLE ) (ffvsp[-7].Node) = New_Unary( DOUBLE, 0, (ffvsp[-7].Node) );
		   if( TYPE((ffvsp[-5].Node))!= DOUBLE ) (ffvsp[-5].Node)= New_Unary( DOUBLE, 0, (ffvsp[-5].Node));
		   if( TYPE((ffvsp[-3].Node))!= DOUBLE ) (ffvsp[-3].Node)= New_Unary( DOUBLE, 0, (ffvsp[-3].Node));
		   if( TYPE((ffvsp[-1].Node))!= DOUBLE ) (ffvsp[-1].Node)= New_Unary( DOUBLE, 0, (ffvsp[-1].Node));
		   if( ! (Test_Dims( (ffvsp[-13].Node), (ffvsp[-11].Node) ) && Test_Dims( (ffvsp[-11].Node), (ffvsp[-9].Node) ) && 
			  Test_Dims( (ffvsp[-9].Node), (ffvsp[-7].Node) ) && Test_Dims( (ffvsp[-7].Node), (ffvsp[-5].Node) ) &&
			  Test_Dims((ffvsp[-5].Node),(ffvsp[-3].Node) ) && Test_Dims((ffvsp[-3].Node), (ffvsp[-1].Node) ) ) ) {
=======
		     if( SIZE((ffval.Node))<SIZE((ffvsp[(2) - (11)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(2) - (11)].Node));
		     if( SIZE((ffvsp[(2) - (11)].Node))<SIZE((ffvsp[(4) - (11)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(4) - (11)].Node));
		     if( SIZE((ffvsp[(4) - (11)].Node))<SIZE((ffvsp[(6) - (11)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(6) - (11)].Node));
		     if( SIZE((ffvsp[(6) - (11)].Node))<SIZE((ffvsp[(8) - (11)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(8) - (11)].Node));
		     if( SIZE((ffvsp[(8) - (11)].Node))<SIZE((ffvsp[(10) - (11)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(10) - (11)].Node));
		   }
		}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 980 "eval.y"
    {
		   if( TYPE((ffvsp[(2) - (15)].Node)) != DOUBLE ) (ffvsp[(2) - (15)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(2) - (15)].Node) );
		   if( TYPE((ffvsp[(4) - (15)].Node)) != DOUBLE ) (ffvsp[(4) - (15)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(4) - (15)].Node) );
		   if( TYPE((ffvsp[(6) - (15)].Node)) != DOUBLE ) (ffvsp[(6) - (15)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(6) - (15)].Node) );
		   if( TYPE((ffvsp[(8) - (15)].Node)) != DOUBLE ) (ffvsp[(8) - (15)].Node) = New_Unary( DOUBLE, 0, (ffvsp[(8) - (15)].Node) );
		   if( TYPE((ffvsp[(10) - (15)].Node))!= DOUBLE ) (ffvsp[(10) - (15)].Node)= New_Unary( DOUBLE, 0, (ffvsp[(10) - (15)].Node));
		   if( TYPE((ffvsp[(12) - (15)].Node))!= DOUBLE ) (ffvsp[(12) - (15)].Node)= New_Unary( DOUBLE, 0, (ffvsp[(12) - (15)].Node));
		   if( TYPE((ffvsp[(14) - (15)].Node))!= DOUBLE ) (ffvsp[(14) - (15)].Node)= New_Unary( DOUBLE, 0, (ffvsp[(14) - (15)].Node));
		   if( ! (Test_Dims( (ffvsp[(2) - (15)].Node), (ffvsp[(4) - (15)].Node) ) && Test_Dims( (ffvsp[(4) - (15)].Node), (ffvsp[(6) - (15)].Node) ) && 
			  Test_Dims( (ffvsp[(6) - (15)].Node), (ffvsp[(8) - (15)].Node) ) && Test_Dims( (ffvsp[(8) - (15)].Node), (ffvsp[(10) - (15)].Node) ) &&
			  Test_Dims((ffvsp[(10) - (15)].Node),(ffvsp[(12) - (15)].Node) ) && Test_Dims((ffvsp[(12) - (15)].Node), (ffvsp[(14) - (15)].Node) ) ) ) {
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     fferror("Dimensions of BOX or ELLIPSE arguments "
			     "are not compatible");
		     FFERROR;
		   } else {
<<<<<<< HEAD
		     if (FSTRCMP((ffvsp[-14].str),"BOX(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, box_fct, 7, (ffvsp[-13].Node), (ffvsp[-11].Node), (ffvsp[-9].Node), (ffvsp[-7].Node),
				      (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node) );
		     } else if (FSTRCMP((ffvsp[-14].str),"ELLIPSE(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, elps_fct, 7, (ffvsp[-13].Node), (ffvsp[-11].Node), (ffvsp[-9].Node), (ffvsp[-7].Node),
				      (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node) );
=======
		     if (FSTRCMP((ffvsp[(1) - (15)].str),"BOX(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, box_fct, 7, (ffvsp[(2) - (15)].Node), (ffvsp[(4) - (15)].Node), (ffvsp[(6) - (15)].Node), (ffvsp[(8) - (15)].Node),
				      (ffvsp[(10) - (15)].Node), (ffvsp[(12) - (15)].Node), (ffvsp[(14) - (15)].Node) );
		     } else if (FSTRCMP((ffvsp[(1) - (15)].str),"ELLIPSE(") == 0) {
		       (ffval.Node) = New_Func( BOOLEAN, elps_fct, 7, (ffvsp[(2) - (15)].Node), (ffvsp[(4) - (15)].Node), (ffvsp[(6) - (15)].Node), (ffvsp[(8) - (15)].Node),
				      (ffvsp[(10) - (15)].Node), (ffvsp[(12) - (15)].Node), (ffvsp[(14) - (15)].Node) );
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     } else {
		       fferror("SAO Image Function not supported");
		       FFERROR;
		     }
		     TEST((ffval.Node)); 
<<<<<<< HEAD
		     if( SIZE((ffval.Node))<SIZE((ffvsp[-13].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-13].Node));
		     if( SIZE((ffvsp[-13].Node))<SIZE((ffvsp[-11].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-11].Node));
		     if( SIZE((ffvsp[-11].Node))<SIZE((ffvsp[-9].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-9].Node));
		     if( SIZE((ffvsp[-9].Node))<SIZE((ffvsp[-7].Node)) )  Copy_Dims((ffval.Node), (ffvsp[-7].Node));
		     if( SIZE((ffvsp[-7].Node))<SIZE((ffvsp[-5].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-5].Node));
		     if( SIZE((ffvsp[-5].Node))<SIZE((ffvsp[-3].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-3].Node));
		     if( SIZE((ffvsp[-3].Node))<SIZE((ffvsp[-1].Node)) ) Copy_Dims((ffval.Node), (ffvsp[-1].Node));
		   }
		}
#line 3092 "y.tab.c"
    break;

  case 106: /* bexpr: GTIFILTER ')'  */
#line 1051 "eval.y"
                { /* Use defaults for all elements */
		   (ffval.Node) = New_GTI(gtifilt_fct,  "", -99, -99, "*START*", "*STOP*" );
                   TEST((ffval.Node));                                        }
#line 3100 "y.tab.c"
    break;

  case 107: /* bexpr: GTIFILTER STRING ')'  */
#line 1055 "eval.y"
                { /* Use defaults for all except filename */
		  (ffval.Node) = New_GTI(gtifilt_fct,  (ffvsp[-1].str), -99, -99, "*START*", "*STOP*" );
                   TEST((ffval.Node));                                        }
#line 3108 "y.tab.c"
    break;

  case 108: /* bexpr: GTIFILTER STRING ',' expr ')'  */
#line 1059 "eval.y"
                {  (ffval.Node) = New_GTI(gtifilt_fct,  (ffvsp[-3].str), (ffvsp[-1].Node), -99, "*START*", "*STOP*" );
                   TEST((ffval.Node));                                        }
#line 3115 "y.tab.c"
    break;

  case 109: /* bexpr: GTIFILTER STRING ',' expr ',' STRING ',' STRING ')'  */
#line 1062 "eval.y"
                {  (ffval.Node) = New_GTI(gtifilt_fct,  (ffvsp[-7].str), (ffvsp[-5].Node), -99, (ffvsp[-3].str), (ffvsp[-1].str) );
                   TEST((ffval.Node));                                        }
#line 3122 "y.tab.c"
    break;

  case 110: /* bexpr: GTIOVERLAP STRING ',' expr ',' expr ')'  */
#line 1067 "eval.y"
                {  (ffval.Node) = New_GTI(gtiover_fct,  (ffvsp[-5].str), (ffvsp[-3].Node), (ffvsp[-1].Node), "*START*", "*STOP*");
                   TEST((ffval.Node));                                        }
#line 3129 "y.tab.c"
    break;

  case 111: /* bexpr: GTIOVERLAP STRING ',' expr ',' expr ',' STRING ',' STRING ')'  */
#line 1070 "eval.y"
                {  (ffval.Node) = New_GTI(gtiover_fct,  (ffvsp[-9].str), (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].str), (ffvsp[-1].str) );
                   TEST((ffval.Node));                                        }
#line 3136 "y.tab.c"
    break;

  case 112: /* bexpr: REGFILTER STRING ')'  */
#line 1075 "eval.y"
                { /* Use defaults for all except filename */
                   (ffval.Node) = New_REG( (ffvsp[-1].str), -99, -99, "" );
                   TEST((ffval.Node));                                        }
#line 3144 "y.tab.c"
    break;

  case 113: /* bexpr: REGFILTER STRING ',' expr ',' expr ')'  */
#line 1079 "eval.y"
                {  (ffval.Node) = New_REG( (ffvsp[-5].str), (ffvsp[-3].Node), (ffvsp[-1].Node), "" );
                   TEST((ffval.Node));                                        }
#line 3151 "y.tab.c"
    break;

  case 114: /* bexpr: REGFILTER STRING ',' expr ',' expr ',' STRING ')'  */
#line 1082 "eval.y"
                {  (ffval.Node) = New_REG( (ffvsp[-7].str), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].str) );
                   TEST((ffval.Node));                                        }
#line 3158 "y.tab.c"
    break;

  case 115: /* bexpr: bexpr '[' expr ']'  */
#line 1086 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-3].Node), 1, (ffvsp[-1].Node),  0,  0,  0,   0 ); TEST((ffval.Node)); }
#line 3164 "y.tab.c"
    break;

  case 116: /* bexpr: bexpr '[' expr ',' expr ']'  */
#line 1088 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-5].Node), 2, (ffvsp[-3].Node), (ffvsp[-1].Node),  0,  0,   0 ); TEST((ffval.Node)); }
#line 3170 "y.tab.c"
    break;

  case 117: /* bexpr: bexpr '[' expr ',' expr ',' expr ']'  */
#line 1090 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-7].Node), 3, (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),  0,   0 ); TEST((ffval.Node)); }
#line 3176 "y.tab.c"
    break;

  case 118: /* bexpr: bexpr '[' expr ',' expr ',' expr ',' expr ']'  */
#line 1092 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-9].Node), 4, (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node),   0 ); TEST((ffval.Node)); }
#line 3182 "y.tab.c"
    break;

  case 119: /* bexpr: bexpr '[' expr ',' expr ',' expr ',' expr ',' expr ']'  */
#line 1094 "eval.y"
                { (ffval.Node) = New_Deref( (ffvsp[-11].Node), 5, (ffvsp[-9].Node), (ffvsp[-7].Node), (ffvsp[-5].Node), (ffvsp[-3].Node), (ffvsp[-1].Node) ); TEST((ffval.Node)); }
#line 3188 "y.tab.c"
    break;

  case 120: /* bexpr: NOT bexpr  */
#line 1096 "eval.y"
                { (ffval.Node) = New_Unary( BOOLEAN, NOT, (ffvsp[0].Node) ); TEST((ffval.Node)); }
#line 3194 "y.tab.c"
    break;

  case 121: /* bexpr: '(' bexpr ')'  */
#line 1098 "eval.y"
                { (ffval.Node) = (ffvsp[-1].Node); }
#line 3200 "y.tab.c"
    break;

  case 122: /* sexpr: STRING  */
#line 1102 "eval.y"
                { (ffval.Node) = New_Const( STRING, (ffvsp[0].str), strlen((ffvsp[0].str))+1 ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = strlen((ffvsp[0].str)); }
#line 3207 "y.tab.c"
    break;

  case 123: /* sexpr: SCOLUMN  */
#line 1105 "eval.y"
                { (ffval.Node) = New_Column( (ffvsp[0].lng) ); TEST((ffval.Node)); }
#line 3213 "y.tab.c"
    break;

  case 124: /* sexpr: SCOLUMN '{' expr '}'  */
#line 1107 "eval.y"
                {
                  if( TYPE((ffvsp[-1].Node)) != LONG
		      || OPER((ffvsp[-1].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[-3].lng), (ffvsp[-1].Node) ); TEST((ffval.Node));
                }
#line 3226 "y.tab.c"
    break;

  case 125: /* sexpr: SNULLREF  */
#line 1116 "eval.y"
                { (ffval.Node) = New_Func( STRING, null_fct, 0, 0, 0, 0, 0, 0, 0, 0 ); }
#line 3232 "y.tab.c"
    break;

  case 126: /* sexpr: '(' sexpr ')'  */
#line 1118 "eval.y"
                { (ffval.Node) = (ffvsp[-1].Node); }
#line 3238 "y.tab.c"
    break;

  case 127: /* sexpr: sexpr '+' sexpr  */
#line 1120 "eval.y"
                { 
		  if (SIZE((ffvsp[-2].Node))+SIZE((ffvsp[0].Node)) >= MAX_STRLEN) {
		    fferror("Combined string size exceeds " MAX_STRLEN_S " characters");
		    FFERROR;
		  }
		  (ffval.Node) = New_BinOp( STRING, (ffvsp[-2].Node), '+', (ffvsp[0].Node) );  TEST((ffval.Node));
		  SIZE((ffval.Node)) = SIZE((ffvsp[-2].Node)) + SIZE((ffvsp[0].Node));
		}
#line 3251 "y.tab.c"
    break;

  case 128: /* sexpr: bexpr '?' sexpr ':' sexpr  */
#line 1129 "eval.y"
                {
		  int outSize;
                  if( SIZE((ffvsp[-4].Node))!=1 ) {
=======
		     if( SIZE((ffval.Node))<SIZE((ffvsp[(2) - (15)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(2) - (15)].Node));
		     if( SIZE((ffvsp[(2) - (15)].Node))<SIZE((ffvsp[(4) - (15)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(4) - (15)].Node));
		     if( SIZE((ffvsp[(4) - (15)].Node))<SIZE((ffvsp[(6) - (15)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(6) - (15)].Node));
		     if( SIZE((ffvsp[(6) - (15)].Node))<SIZE((ffvsp[(8) - (15)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(8) - (15)].Node));
		     if( SIZE((ffvsp[(8) - (15)].Node))<SIZE((ffvsp[(10) - (15)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(10) - (15)].Node));
		     if( SIZE((ffvsp[(10) - (15)].Node))<SIZE((ffvsp[(12) - (15)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(12) - (15)].Node));
		     if( SIZE((ffvsp[(12) - (15)].Node))<SIZE((ffvsp[(14) - (15)].Node)) ) Copy_Dims((ffval.Node), (ffvsp[(14) - (15)].Node));
		   }
		}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1017 "eval.y"
    { /* Use defaults for all elements */
                   (ffval.Node) = New_GTI( "", -99, "*START*", "*STOP*" );
                   TEST((ffval.Node));                                        }
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1021 "eval.y"
    { /* Use defaults for all except filename */
                   (ffval.Node) = New_GTI( (ffvsp[(2) - (3)].str), -99, "*START*", "*STOP*" );
                   TEST((ffval.Node));                                        }
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 1025 "eval.y"
    {  (ffval.Node) = New_GTI( (ffvsp[(2) - (5)].str), (ffvsp[(4) - (5)].Node), "*START*", "*STOP*" );
                   TEST((ffval.Node));                                        }
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1028 "eval.y"
    {  (ffval.Node) = New_GTI( (ffvsp[(2) - (9)].str), (ffvsp[(4) - (9)].Node), (ffvsp[(6) - (9)].str), (ffvsp[(8) - (9)].str) );
                   TEST((ffval.Node));                                        }
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1032 "eval.y"
    { /* Use defaults for all except filename */
                   (ffval.Node) = New_REG( (ffvsp[(2) - (3)].str), -99, -99, "" );
                   TEST((ffval.Node));                                        }
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 1036 "eval.y"
    {  (ffval.Node) = New_REG( (ffvsp[(2) - (7)].str), (ffvsp[(4) - (7)].Node), (ffvsp[(6) - (7)].Node), "" );
                   TEST((ffval.Node));                                        }
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 1039 "eval.y"
    {  (ffval.Node) = New_REG( (ffvsp[(2) - (9)].str), (ffvsp[(4) - (9)].Node), (ffvsp[(6) - (9)].Node), (ffvsp[(8) - (9)].str) );
                   TEST((ffval.Node));                                        }
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 1043 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (4)].Node), 1, (ffvsp[(3) - (4)].Node),  0,  0,  0,   0 ); TEST((ffval.Node)); }
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 1045 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (6)].Node), 2, (ffvsp[(3) - (6)].Node), (ffvsp[(5) - (6)].Node),  0,  0,   0 ); TEST((ffval.Node)); }
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 1047 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (8)].Node), 3, (ffvsp[(3) - (8)].Node), (ffvsp[(5) - (8)].Node), (ffvsp[(7) - (8)].Node),  0,   0 ); TEST((ffval.Node)); }
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 1049 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (10)].Node), 4, (ffvsp[(3) - (10)].Node), (ffvsp[(5) - (10)].Node), (ffvsp[(7) - (10)].Node), (ffvsp[(9) - (10)].Node),   0 ); TEST((ffval.Node)); }
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 1051 "eval.y"
    { (ffval.Node) = New_Deref( (ffvsp[(1) - (12)].Node), 5, (ffvsp[(3) - (12)].Node), (ffvsp[(5) - (12)].Node), (ffvsp[(7) - (12)].Node), (ffvsp[(9) - (12)].Node), (ffvsp[(11) - (12)].Node) ); TEST((ffval.Node)); }
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 1053 "eval.y"
    { (ffval.Node) = New_Unary( BOOLEAN, NOT, (ffvsp[(2) - (2)].Node) ); TEST((ffval.Node)); }
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 1055 "eval.y"
    { (ffval.Node) = (ffvsp[(2) - (3)].Node); }
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1059 "eval.y"
    { (ffval.Node) = New_Const( STRING, (ffvsp[(1) - (1)].str), strlen((ffvsp[(1) - (1)].str))+1 ); TEST((ffval.Node));
                  SIZE((ffval.Node)) = strlen((ffvsp[(1) - (1)].str)); }
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1062 "eval.y"
    { (ffval.Node) = New_Column( (ffvsp[(1) - (1)].lng) ); TEST((ffval.Node)); }
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1064 "eval.y"
    {
                  if( TYPE((ffvsp[(3) - (4)].Node)) != LONG
		      || OPER((ffvsp[(3) - (4)].Node)) != CONST_OP ) {
		     fferror("Offset argument must be a constant integer");
		     FFERROR;
		  }
                  (ffval.Node) = New_Offset( (ffvsp[(1) - (4)].lng), (ffvsp[(3) - (4)].Node) ); TEST((ffval.Node));
                }
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1073 "eval.y"
    { (ffval.Node) = New_Func( STRING, null_fct, 0, 0, 0, 0, 0, 0, 0, 0 ); }
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1075 "eval.y"
    { (ffval.Node) = (ffvsp[(2) - (3)].Node); }
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1077 "eval.y"
    { 
		  if (SIZE((ffvsp[(1) - (3)].Node))+SIZE((ffvsp[(3) - (3)].Node)) >= MAX_STRLEN) {
		    fferror("Combined string size exceeds " MAX_STRLEN_S " characters");
		    FFERROR;
		  }
		  (ffval.Node) = New_BinOp( STRING, (ffvsp[(1) - (3)].Node), '+', (ffvsp[(3) - (3)].Node) );  TEST((ffval.Node));
		  SIZE((ffval.Node)) = SIZE((ffvsp[(1) - (3)].Node)) + SIZE((ffvsp[(3) - (3)].Node));
		}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1086 "eval.y"
    {
		  int outSize;
                  if( SIZE((ffvsp[(1) - (5)].Node))!=1 ) {
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
                     fferror("Cannot have a vector string column");
		     FFERROR;
                  }
		  /* Since the output can be calculated now, as a constant
		     scalar, we must precalculate the output size, in
		     order to avoid an overflow. */
<<<<<<< HEAD
		  outSize = SIZE((ffvsp[-2].Node));
		  if (SIZE((ffvsp[0].Node)) > outSize) outSize = SIZE((ffvsp[0].Node));
                  (ffval.Node) = New_FuncSize( 0, ifthenelse_fct, 3, (ffvsp[-2].Node), (ffvsp[0].Node), (ffvsp[-4].Node),
				     0, 0, 0, 0, outSize);
		  
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[-2].Node))<SIZE((ffvsp[0].Node)) )  Copy_Dims((ffval.Node), (ffvsp[0].Node));
                }
#line 3273 "y.tab.c"
    break;

  case 129: /* sexpr: FUNCTION sexpr ',' sexpr ')'  */
#line 1148 "eval.y"
                { 
		  if (FSTRCMP((ffvsp[-4].str),"DEFNULL(") == 0) {
=======
		  outSize = SIZE((ffvsp[(3) - (5)].Node));
		  if (SIZE((ffvsp[(5) - (5)].Node)) > outSize) outSize = SIZE((ffvsp[(5) - (5)].Node));
                  (ffval.Node) = New_FuncSize( 0, ifthenelse_fct, 3, (ffvsp[(3) - (5)].Node), (ffvsp[(5) - (5)].Node), (ffvsp[(1) - (5)].Node),
				     0, 0, 0, 0, outSize);
		  
                  TEST((ffval.Node));
                  if( SIZE((ffvsp[(3) - (5)].Node))<SIZE((ffvsp[(5) - (5)].Node)) )  Copy_Dims((ffval.Node), (ffvsp[(5) - (5)].Node));
                }
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1105 "eval.y"
    { 
		  if (FSTRCMP((ffvsp[(1) - (5)].str),"DEFNULL(") == 0) {
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		     int outSize;
		     /* Since the output can be calculated now, as a constant
			scalar, we must precalculate the output size, in
			order to avoid an overflow. */
<<<<<<< HEAD
		     outSize = SIZE((ffvsp[-3].Node));
		     if (SIZE((ffvsp[-1].Node)) > outSize) outSize = SIZE((ffvsp[-1].Node));
		     
		     (ffval.Node) = New_FuncSize( 0, defnull_fct, 2, (ffvsp[-3].Node), (ffvsp[-1].Node), 0,
					0, 0, 0, 0, outSize );
		     TEST((ffval.Node)); 
		     if( SIZE((ffvsp[-1].Node))>SIZE((ffvsp[-3].Node)) ) SIZE((ffval.Node)) = SIZE((ffvsp[-1].Node));
=======
		     outSize = SIZE((ffvsp[(2) - (5)].Node));
		     if (SIZE((ffvsp[(4) - (5)].Node)) > outSize) outSize = SIZE((ffvsp[(4) - (5)].Node));
		     
		     (ffval.Node) = New_FuncSize( 0, defnull_fct, 2, (ffvsp[(2) - (5)].Node), (ffvsp[(4) - (5)].Node), 0,
					0, 0, 0, 0, outSize );
		     TEST((ffval.Node)); 
		     if( SIZE((ffvsp[(4) - (5)].Node))>SIZE((ffvsp[(2) - (5)].Node)) ) SIZE((ffval.Node)) = SIZE((ffvsp[(4) - (5)].Node));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		  } else {
		     fferror("Function(string,string) not supported");
		     FFERROR;
		  }
		}
<<<<<<< HEAD
#line 3296 "y.tab.c"
    break;

  case 130: /* sexpr: FUNCTION sexpr ',' expr ',' expr ')'  */
#line 1167 "eval.y"
                { 
		  if (FSTRCMP((ffvsp[-6].str),"STRMID(") == 0) {
		    int len;
		    if( TYPE((ffvsp[-3].Node)) != LONG || SIZE((ffvsp[-3].Node)) != 1 ||
			TYPE((ffvsp[-1].Node)) != LONG || SIZE((ffvsp[-1].Node)) != 1) {
		      fferror("When using STRMID(S,P,N), P and N must be integers (and not vector columns)");
		      FFERROR;
		    }
		    if (OPER((ffvsp[-1].Node)) == CONST_OP) {
		      /* Constant value: use that directly */
		      len = (gParse.Nodes[(ffvsp[-1].Node)].value.data.lng);
		    } else {
		      /* Variable value: use the maximum possible (from $2) */
		      len = SIZE((ffvsp[-5].Node));
=======
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1124 "eval.y"
    { 
		  if (FSTRCMP((ffvsp[(1) - (7)].str),"STRMID(") == 0) {
		    int len;
		    if( TYPE((ffvsp[(4) - (7)].Node)) != LONG || SIZE((ffvsp[(4) - (7)].Node)) != 1 ||
			TYPE((ffvsp[(6) - (7)].Node)) != LONG || SIZE((ffvsp[(6) - (7)].Node)) != 1) {
		      fferror("When using STRMID(S,P,N), P and N must be integers (and not vector columns)");
		      FFERROR;
		    }
		    if (OPER((ffvsp[(6) - (7)].Node)) == CONST_OP) {
		      /* Constant value: use that directly */
		      len = (gParse.Nodes[(ffvsp[(6) - (7)].Node)].value.data.lng);
		    } else {
		      /* Variable value: use the maximum possible (from $2) */
		      len = SIZE((ffvsp[(2) - (7)].Node));
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		    }
		    if (len <= 0 || len >= MAX_STRLEN) {
		      fferror("STRMID(S,P,N), N must be 1-" MAX_STRLEN_S);
		      FFERROR;
		    }
<<<<<<< HEAD
		    (ffval.Node) = New_FuncSize( 0, strmid_fct, 3, (ffvsp[-5].Node), (ffvsp[-3].Node),(ffvsp[-1].Node),0,0,0,0,len);
=======
		    (ffval.Node) = New_FuncSize( 0, strmid_fct, 3, (ffvsp[(2) - (7)].Node), (ffvsp[(4) - (7)].Node),(ffvsp[(6) - (7)].Node),0,0,0,0,len);
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
		    TEST((ffval.Node));
		  } else {
		     fferror("Function(string,expr,expr) not supported");
		     FFERROR;
		  }
		}
<<<<<<< HEAD
#line 3327 "y.tab.c"
    break;


#line 3331 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter ffchar, and that requires
     that fftoken be updated with the new translation.  We take the
     approach of translating immediately before every use of fftoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     FFABORT, FFACCEPT, or FFERROR immediately after altering ffchar or
     if it invokes FFBACKUP.  In the case of FFABORT or FFACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of FFERROR or FFBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  FF_SYMBOL_PRINT ("-> $$ =", FF_CAST (ffsymbol_kind_t, ffr1[ffn]), &ffval, &ffloc);

  FFPOPSTACK (fflen);
  fflen = 0;

  *++ffvsp = ffval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int fflhs = ffr1[ffn] - FFNTOKENS;
    const int ffi = ffpgoto[fflhs] + *ffssp;
    ffstate = (0 <= ffi && ffi <= FFLAST && ffcheck[ffi] == *ffssp
               ? fftable[ffi]
               : ffdefgoto[fflhs]);
  }
=======
    break;



/* Line 1455 of yacc.c  */
#line 3584 "y.tab.c"
      default: break;
    }
  FF_SYMBOL_PRINT ("-> $$ =", ffr1[ffn], &ffval, &ffloc);

  FFPOPSTACK (fflen);
  fflen = 0;
  FF_STACK_PRINT (ffss, ffssp);

  *++ffvsp = ffval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  ffn = ffr1[ffn];

  ffstate = ffpgoto[ffn - FFNTOKENS] + *ffssp;
  if (0 <= ffstate && ffstate <= FFLAST && ffcheck[ffstate] == *ffssp)
    ffstate = fftable[ffstate];
  else
    ffstate = ffdefgoto[ffn - FFNTOKENS];
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0

  goto ffnewstate;


<<<<<<< HEAD
/*--------------------------------------.
| fferrlab -- here on detecting error.  |
`--------------------------------------*/
fferrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  fftoken = ffchar == FFEMPTY ? FFSYMBOL_FFEMPTY : FFTRANSLATE (ffchar);
=======
/*------------------------------------.
| fferrlab -- here on detecting error |
`------------------------------------*/
fferrlab:
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
  /* If not already recovering from an error, report this error.  */
  if (!fferrstatus)
    {
      ++ffnerrs;
<<<<<<< HEAD
      fferror (FF_("syntax error"));
    }

  if (fferrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (ffchar <= FFEOF)
        {
          /* Return failure if at end of input.  */
          if (ffchar == FFEOF)
            FFABORT;
        }
      else
        {
          ffdestruct ("Error: discarding",
                      fftoken, &fflval);
          ffchar = FFEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto fferrlab1;


/*---------------------------------------------------.
| fferrorlab -- error raised explicitly by FFERROR.  |
`---------------------------------------------------*/
fferrorlab:
  /* Pacify compilers when the user code never invokes FFERROR and the
     label fferrorlab therefore never appears in user code.  */
  if (0)
    FFERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this FFERROR.  */
  FFPOPSTACK (fflen);
  fflen = 0;
  FF_STACK_PRINT (ffss, ffssp);
  ffstate = *ffssp;
  goto fferrlab1;


/*-------------------------------------------------------------.
| fferrlab1 -- common code for both syntax error and FFERROR.  |
`-------------------------------------------------------------*/
fferrlab1:
  fferrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      ffn = ffpact[ffstate];
      if (!ffpact_value_is_default (ffn))
        {
          ffn += FFSYMBOL_FFerror;
          if (0 <= ffn && ffn <= FFLAST && ffcheck[ffn] == FFSYMBOL_FFerror)
            {
              ffn = fftable[ffn];
              if (0 < ffn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (ffssp == ffss)
        FFABORT;


      ffdestruct ("Error: popping",
                  FF_ACCESSING_SYMBOL (ffstate), ffvsp);
      FFPOPSTACK (1);
      ffstate = *ffssp;
      FF_STACK_PRINT (ffss, ffssp);
    }

  FF_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++ffvsp = fflval;
  FF_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  FF_SYMBOL_PRINT ("Shifting", FF_ACCESSING_SYMBOL (ffn), ffvsp, fflsp);
=======
#if ! FFERROR_VERBOSE
      fferror (FF_("syntax error"));
#else
      {
	FFSIZE_T ffsize = ffsyntax_error (0, ffstate, ffchar);
	if (ffmsg_alloc < ffsize && ffmsg_alloc < FFSTACK_ALLOC_MAXIMUM)
	  {
	    FFSIZE_T ffalloc = 2 * ffsize;
	    if (! (ffsize <= ffalloc && ffalloc <= FFSTACK_ALLOC_MAXIMUM))
	      ffalloc = FFSTACK_ALLOC_MAXIMUM;
	    if (ffmsg != ffmsgbuf)
	      FFSTACK_FREE (ffmsg);
	    ffmsg = (char *) FFSTACK_ALLOC (ffalloc);
	    if (ffmsg)
	      ffmsg_alloc = ffalloc;
	    else
	      {
		ffmsg = ffmsgbuf;
		ffmsg_alloc = sizeof ffmsgbuf;
	      }
	  }

	if (0 < ffsize && ffsize <= ffmsg_alloc)
	  {
	    (void) ffsyntax_error (ffmsg, ffstate, ffchar);
	    fferror (ffmsg);
	  }
	else
	  {
	    fferror (FF_("syntax error"));
	    if (ffsize != 0)
	      goto ffexhaustedlab;
	  }
      }
#endif
    }



  if (fferrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (ffchar <= FFEOF)
	{
	  /* Return failure if at end of input.  */
	  if (ffchar == FFEOF)
	    FFABORT;
	}
      else
	{
	  ffdestruct ("Error: discarding",
		      fftoken, &fflval);
	  ffchar = FFEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto fferrlab1;


/*---------------------------------------------------.
| fferrorlab -- error raised explicitly by FFERROR.  |
`---------------------------------------------------*/
fferrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     FFERROR and the label fferrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto fferrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this FFERROR.  */
  FFPOPSTACK (fflen);
  fflen = 0;
  FF_STACK_PRINT (ffss, ffssp);
  ffstate = *ffssp;
  goto fferrlab1;


/*-------------------------------------------------------------.
| fferrlab1 -- common code for both syntax error and FFERROR.  |
`-------------------------------------------------------------*/
fferrlab1:
  fferrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      ffn = ffpact[ffstate];
      if (ffn != FFPACT_NINF)
	{
	  ffn += FFTERROR;
	  if (0 <= ffn && ffn <= FFLAST && ffcheck[ffn] == FFTERROR)
	    {
	      ffn = fftable[ffn];
	      if (0 < ffn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (ffssp == ffss)
	FFABORT;


      ffdestruct ("Error: popping",
		  ffstos[ffstate], ffvsp);
      FFPOPSTACK (1);
      ffstate = *ffssp;
      FF_STACK_PRINT (ffss, ffssp);
    }

  *++ffvsp = fflval;


  /* Shift the error token.  */
  FF_SYMBOL_PRINT ("Shifting", ffstos[ffn], ffvsp, fflsp);
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0

  ffstate = ffn;
  goto ffnewstate;


/*-------------------------------------.
| ffacceptlab -- FFACCEPT comes here.  |
`-------------------------------------*/
ffacceptlab:
  ffresult = 0;
  goto ffreturn;

<<<<<<< HEAD

=======
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
/*-----------------------------------.
| ffabortlab -- FFABORT comes here.  |
`-----------------------------------*/
ffabortlab:
  ffresult = 1;
  goto ffreturn;

<<<<<<< HEAD

#if !defined ffoverflow
=======
#if !defined(ffoverflow) || FFERROR_VERBOSE
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
/*-------------------------------------------------.
| ffexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
ffexhaustedlab:
  fferror (FF_("memory exhausted"));
  ffresult = 2;
<<<<<<< HEAD
  goto ffreturn;
#endif


/*-------------------------------------------------------.
| ffreturn -- parsing is finished, clean up and return.  |
`-------------------------------------------------------*/
ffreturn:
  if (ffchar != FFEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      fftoken = FFTRANSLATE (ffchar);
      ffdestruct ("Cleanup: discarding lookahead",
                  fftoken, &fflval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
=======
  /* Fall through.  */
#endif

ffreturn:
  if (ffchar != FFEMPTY)
     ffdestruct ("Cleanup: discarding lookahead",
		 fftoken, &fflval);
  /* Do not reclaim the symbols of the rule which action triggered
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
     this FFABORT or FFACCEPT.  */
  FFPOPSTACK (fflen);
  FF_STACK_PRINT (ffss, ffssp);
  while (ffssp != ffss)
    {
      ffdestruct ("Cleanup: popping",
<<<<<<< HEAD
                  FF_ACCESSING_SYMBOL (+*ffssp), ffvsp);
=======
		  ffstos[*ffssp], ffvsp);
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
      FFPOPSTACK (1);
    }
#ifndef ffoverflow
  if (ffss != ffssa)
    FFSTACK_FREE (ffss);
#endif
<<<<<<< HEAD

  return ffresult;
}

#line 1196 "eval.y"
=======
#if FFERROR_VERBOSE
  if (ffmsg != ffmsgbuf)
    FFSTACK_FREE (ffmsg);
#endif
  /* Make sure FFID is used.  */
  return FFID (ffresult);
}



/* Line 1675 of yacc.c  */
#line 1153 "eval.y"
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0


/*************************************************************************/
/*  Start of "New" routines which build the expression Nodal structure   */
/*************************************************************************/

static int Alloc_Node( void )
{
                      /* Use this for allocation to guarantee *Nodes */
   Node *newNodePtr;  /* survives on failure, making it still valid  */
                      /* while working our way out of this error     */

   if( gParse.nNodes == gParse.nNodesAlloc ) {
      if( gParse.Nodes ) {
	 gParse.nNodesAlloc += gParse.nNodesAlloc;
	 newNodePtr = (Node *)realloc( gParse.Nodes,
				       sizeof(Node)*gParse.nNodesAlloc );
      } else {
	 gParse.nNodesAlloc = 100;
	 newNodePtr = (Node *)malloc ( sizeof(Node)*gParse.nNodesAlloc );
      }	 

      if( newNodePtr ) {
	 gParse.Nodes = newNodePtr;
      } else {
	 gParse.status = MEMORY_ALLOCATION;
	 return( -1 );
      }
   }

   return ( gParse.nNodes++ );
}

static void Free_Last_Node( void )
{
   if( gParse.nNodes ) gParse.nNodes--;
}

static int New_Const( int returnType, void *value, long len )
{
   Node *this;
   int n;

   n = Alloc_Node();
   if( n>=0 ) {
      this             = gParse.Nodes + n;
      this->operation  = CONST_OP;             /* Flag a constant */
      this->DoOp       = NULL;
      this->nSubNodes  = 0;
      this->type       = returnType;
      memcpy( &(this->value.data), value, len );
      this->value.undef = NULL;
      this->value.nelem = 1;
      this->value.naxis = 1;
      this->value.naxes[0] = 1;
   }
   return(n);
}

static int New_Column( int ColNum )
{
   Node *this;
   int  n, i;

   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->operation   = -ColNum;
      this->DoOp        = NULL;
      this->nSubNodes   = 0;
      this->type        = gParse.varData[ColNum].type;
      this->value.nelem = gParse.varData[ColNum].nelem;
      this->value.naxis = gParse.varData[ColNum].naxis;
      for( i=0; i<gParse.varData[ColNum].naxis; i++ )
	 this->value.naxes[i] = gParse.varData[ColNum].naxes[i];
   }
   return(n);
}

static int New_Offset( int ColNum, int offsetNode )
{
   Node *this;
   int  n, i, colNode;

   colNode = New_Column( ColNum );
   if( colNode<0 ) return(-1);

   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->operation   = '{';
      this->DoOp        = Do_Offset;
      this->nSubNodes   = 2;
      this->SubNodes[0] = colNode;
      this->SubNodes[1] = offsetNode;
      this->type        = gParse.varData[ColNum].type;
      this->value.nelem = gParse.varData[ColNum].nelem;
      this->value.naxis = gParse.varData[ColNum].naxis;
      for( i=0; i<gParse.varData[ColNum].naxis; i++ )
	 this->value.naxes[i] = gParse.varData[ColNum].naxes[i];
   }
   return(n);
}

static int New_Unary( int returnType, int Op, int Node1 )
{
   Node *this, *that;
   int  i,n;

   if( Node1<0 ) return(-1);
   that = gParse.Nodes + Node1;

   if( !Op ) Op = returnType;

   if( (Op==DOUBLE || Op==FLTCAST) && that->type==DOUBLE  ) return( Node1 );
   if( (Op==LONG   || Op==INTCAST) && that->type==LONG    ) return( Node1 );
   if( (Op==BOOLEAN              ) && that->type==BOOLEAN ) return( Node1 );
   
   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->operation   = Op;
      this->DoOp        = Do_Unary;
      this->nSubNodes   = 1;
      this->SubNodes[0] = Node1;
      this->type        = returnType;

      that              = gParse.Nodes + Node1; /* Reset in case .Nodes mv'd */
      this->value.nelem = that->value.nelem;
      this->value.naxis = that->value.naxis;
      for( i=0; i<that->value.naxis; i++ )
	 this->value.naxes[i] = that->value.naxes[i];

      if( that->operation==CONST_OP ) this->DoOp( this );
   }
   return( n );
}

static int New_BinOp( int returnType, int Node1, int Op, int Node2 )
{
   Node *this,*that1,*that2;
   int  n,i,constant;

   if( Node1<0 || Node2<0 ) return(-1);

   n = Alloc_Node();
   if( n>=0 ) {
      this             = gParse.Nodes + n;
      this->operation  = Op;
      this->nSubNodes  = 2;
      this->SubNodes[0]= Node1;
      this->SubNodes[1]= Node2;
      this->type       = returnType;

      that1            = gParse.Nodes + Node1;
      that2            = gParse.Nodes + Node2;
      constant         = (that1->operation==CONST_OP
                          && that2->operation==CONST_OP);
      if( that1->type!=STRING && that1->type!=BITSTR )
	 if( !Test_Dims( Node1, Node2 ) ) {
	    Free_Last_Node();
	    fferror("Array sizes/dims do not match for binary operator");
	    return(-1);
	 }
      if( that1->value.nelem == 1 ) that1 = that2;

      this->value.nelem = that1->value.nelem;
      this->value.naxis = that1->value.naxis;
      for( i=0; i<that1->value.naxis; i++ )
	 this->value.naxes[i] = that1->value.naxes[i];

      if ( Op == ACCUM && that1->type == BITSTR ) {
	/* ACCUM is rank-reducing on bit strings */
	this->value.nelem = 1;
	this->value.naxis = 1;
	this->value.naxes[0] = 1;
      }

      /*  Both subnodes should be of same time  */
      switch( that1->type ) {
      case BITSTR:  this->DoOp = Do_BinOp_bit;  break;
      case STRING:  this->DoOp = Do_BinOp_str;  break;
      case BOOLEAN: this->DoOp = Do_BinOp_log;  break;
      case LONG:    this->DoOp = Do_BinOp_lng;  break;
      case DOUBLE:  this->DoOp = Do_BinOp_dbl;  break;
      }
      if( constant ) this->DoOp( this );
   }
   return( n );
}

static int New_Func( int returnType, funcOp Op, int nNodes,
		     int Node1, int Node2, int Node3, int Node4, 
		     int Node5, int Node6, int Node7 )
{
  return New_FuncSize(returnType, Op, nNodes,
		      Node1, Node2, Node3, Node4, 
		      Node5, Node6, Node7, 0);
}

static int New_FuncSize( int returnType, funcOp Op, int nNodes,
		     int Node1, int Node2, int Node3, int Node4, 
			 int Node5, int Node6, int Node7, int Size )
/* If returnType==0 , use Node1's type and vector sizes as returnType, */
/* else return a single value of type returnType                       */
{
   Node *this, *that;
   int  i,n,constant;

   if( Node1<0 || Node2<0 || Node3<0 || Node4<0 || 
       Node5<0 || Node6<0 || Node7<0 ) return(-1);

   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->operation   = (int)Op;
      this->DoOp        = Do_Func;
      this->nSubNodes   = nNodes;
      this->SubNodes[0] = Node1;
      this->SubNodes[1] = Node2;
      this->SubNodes[2] = Node3;
      this->SubNodes[3] = Node4;
      this->SubNodes[4] = Node5;
      this->SubNodes[5] = Node6;
      this->SubNodes[6] = Node7;
      i = constant = nNodes;    /* Functions with zero params are not const */
      if (Op == poirnd_fct) constant = 0; /* Nor is Poisson deviate */

      while( i-- )
	constant = ( constant && OPER(this->SubNodes[i]) == CONST_OP );
      
      if( returnType ) {
	 this->type           = returnType;
	 this->value.nelem    = 1;
	 this->value.naxis    = 1;
	 this->value.naxes[0] = 1;
      } else {
	 that              = gParse.Nodes + Node1;
	 this->type        = that->type;
	 this->value.nelem = that->value.nelem;
	 this->value.naxis = that->value.naxis;
	 for( i=0; i<that->value.naxis; i++ )
	    this->value.naxes[i] = that->value.naxes[i];
      }
      /* Force explicit size before evaluating */
      if (Size > 0) this->value.nelem = Size;

      if( constant ) this->DoOp( this );
   }
   return( n );
}

static int New_Deref( int Var,  int nDim,
		      int Dim1, int Dim2, int Dim3, int Dim4, int Dim5 )
{
   int n, idx, constant;
   long elem=0;
   Node *this, *theVar, *theDim[MAXDIMS];

   if( Var<0 || Dim1<0 || Dim2<0 || Dim3<0 || Dim4<0 || Dim5<0 ) return(-1);

   theVar = gParse.Nodes + Var;
   if( theVar->operation==CONST_OP || theVar->value.nelem==1 ) {
      fferror("Cannot index a scalar value");
      return(-1);
   }

   n = Alloc_Node();
   if( n>=0 ) {
      this              = gParse.Nodes + n;
      this->nSubNodes   = nDim+1;
      theVar            = gParse.Nodes + (this->SubNodes[0]=Var);
      theDim[0]         = gParse.Nodes + (this->SubNodes[1]=Dim1);
      theDim[1]         = gParse.Nodes + (this->SubNodes[2]=Dim2);
      theDim[2]         = gParse.Nodes + (this->SubNodes[3]=Dim3);
      theDim[3]         = gParse.Nodes + (this->SubNodes[4]=Dim4);
      theDim[4]         = gParse.Nodes + (this->SubNodes[5]=Dim5);
      constant          = theVar->operation==CONST_OP;
      for( idx=0; idx<nDim; idx++ )
	 constant = (constant && theDim[idx]->operation==CONST_OP);

      for( idx=0; idx<nDim; idx++ )
	 if( theDim[idx]->value.nelem>1 ) {
	    Free_Last_Node();
	    fferror("Cannot use an array as an index value");
	    return(-1);
	 } else if( theDim[idx]->type!=LONG ) {
	    Free_Last_Node();
	    fferror("Index value must be an integer type");
	    return(-1);
	 }

      this->operation   = '[';
      this->DoOp        = Do_Deref;
      this->type        = theVar->type;

      if( theVar->value.naxis == nDim ) { /* All dimensions specified */
	 this->value.nelem    = 1;
	 this->value.naxis    = 1;
	 this->value.naxes[0] = 1;
      } else if( nDim==1 ) { /* Dereference only one dimension */
	 elem=1;
	 this->value.naxis = theVar->value.naxis-1;
	 for( idx=0; idx<this->value.naxis; idx++ ) {
	    elem *= ( this->value.naxes[idx] = theVar->value.naxes[idx] );
	 }
	 this->value.nelem = elem;
      } else {
	 Free_Last_Node();
	 fferror("Must specify just one or all indices for vector");
	 return(-1);
      }
      if( constant ) this->DoOp( this );
   }
   return(n);
}

extern int ffGetVariable( char *varName, FFSTYPE *varVal );

static int New_GTI( funcOp Op, char *fname, int Node1, int Node2, char *start, char *stop )
{
   fitsfile *fptr;
   Node *this, *that0, *that1, *that2;
   int  type,i,n, startCol, stopCol, Node0;
   int  hdutype, hdunum, evthdu, samefile, extvers, movetotype, tstat;
   char extname[100];
   long nrows;
   double timeZeroI[2], timeZeroF[2], dt, timeSpan;
   char xcol[20], xexpr[20];
   FFSTYPE colVal;

   if( Op == gtifilt_fct && Node1==-99 ) {
      type = ffGetVariable( "TIME", &colVal );
      if( type==COLUMN ) {
	 Node1 = New_Column( (int)colVal.lng );
      } else {
	 fferror("Could not build TIME column for GTIFILTER");
	 return(-1);
      }
   }

   if (Op == gtiover_fct) {
     if (Node1 == -99 || Node2 == -99) {
       fferror("startExpr and stopExpr values must be defined for GTIOVERLAP");
       return(-1);
     }
     /* Also case TIME_STOP to double precision */
     Node2 = New_Unary( DOUBLE, 0, Node2 );
     if (Node2 < 0) return(-1);

   }

   /* Type cast TIME to double precision */
   Node1 = New_Unary( DOUBLE, 0, Node1 );
   Node0 = Alloc_Node(); /* This will hold the START/STOP times */
   if( Node1<0 || Node0<0 ) return(-1);

   /*  Record current HDU number in case we need to move within this file  */

   fptr = gParse.def_fptr;
   ffghdn( fptr, &evthdu );

   /*  Look for TIMEZERO keywords in current extension  */

   tstat = 0;
   if( ffgkyd( fptr, "TIMEZERO", timeZeroI, NULL, &tstat ) ) {
      tstat = 0;
      if( ffgkyd( fptr, "TIMEZERI", timeZeroI, NULL, &tstat ) ) {
	 timeZeroI[0] = timeZeroF[0] = 0.0;
      } else if( ffgkyd( fptr, "TIMEZERF", timeZeroF, NULL, &tstat ) ) {
	 timeZeroF[0] = 0.0;
      }
   } else {
      timeZeroF[0] = 0.0;
   }

   /*  Resolve filename parameter  */

   switch( fname[0] ) {
   case '\0':
      samefile = 1;
      hdunum = 1;
      break;
   case '[':
      samefile = 1;
      i = 1;
      while( fname[i] != '\0' && fname[i] != ']' ) i++;
      if( fname[i] ) {
	 fname[i] = '\0';
	 fname++;
	 ffexts( fname, &hdunum, extname, &extvers, &movetotype,
		 xcol, xexpr, &gParse.status );
         if( *extname ) {
	    ffmnhd( fptr, movetotype, extname, extvers, &gParse.status );
	    ffghdn( fptr, &hdunum );
	 } else if( hdunum ) {
	    ffmahd( fptr, ++hdunum, &hdutype, &gParse.status );
	 } else if( !gParse.status ) {
	    fferror("Cannot use primary array for GTI filter");
	    return( -1 );
	 }
      } else {
	 fferror("File extension specifier lacks closing ']'");
	 return( -1 );
      }
      break;
   case '+':
      samefile = 1;
      hdunum = atoi( fname ) + 1;
      if( hdunum>1 )
	 ffmahd( fptr, hdunum, &hdutype, &gParse.status );
      else {
	 fferror("Cannot use primary array for GTI filter");
	 return( -1 );
      }
      break;
   default:
      samefile = 0;
      if( ! ffopen( &fptr, fname, READONLY, &gParse.status ) )
	 ffghdn( fptr, &hdunum );
      break;
   }
   if( gParse.status ) return(-1);

   /*  If at primary, search for GTI extension  */

   if( hdunum==1 ) {
      while( 1 ) {
	 hdunum++;
	 if( ffmahd( fptr, hdunum, &hdutype, &gParse.status ) ) break;
	 if( hdutype==IMAGE_HDU ) continue;
	 tstat = 0;
	 if( ffgkys( fptr, "EXTNAME", extname, NULL, &tstat ) ) continue;
	 ffupch( extname );
	 if( strstr( extname, "GTI" ) ) break;
      }
      if( gParse.status ) {
	 if( gParse.status==END_OF_FILE )
	    fferror("GTI extension not found in this file");
	 return(-1);
      }
   }

   /*  Locate START/STOP Columns  */

   ffgcno( fptr, CASEINSEN, start, &startCol, &gParse.status );
   ffgcno( fptr, CASEINSEN, stop,  &stopCol,  &gParse.status );
   if( gParse.status ) return(-1);

   /*  Look for TIMEZERO keywords in GTI extension  */

   tstat = 0;
   if( ffgkyd( fptr, "TIMEZERO", timeZeroI+1, NULL, &tstat ) ) {
      tstat = 0;
      if( ffgkyd( fptr, "TIMEZERI", timeZeroI+1, NULL, &tstat ) ) {
	 timeZeroI[1] = timeZeroF[1] = 0.0;
      } else if( ffgkyd( fptr, "TIMEZERF", timeZeroF+1, NULL, &tstat ) ) {
	 timeZeroF[1] = 0.0;
      }
   } else {
      timeZeroF[1] = 0.0;
   }

   n = Alloc_Node();
   if( n >= 0 ) {
      this                 = gParse.Nodes + n;
      this->SubNodes[1]    = Node1;
      this->operation      = (int) Op;
      if (Op == gtifilt_fct) {
	this->nSubNodes      = 2;
	this->DoOp           = Do_GTI;
	this->type           = BOOLEAN;
      } else {
	this->nSubNodes      = 3;
	this->DoOp           = Do_GTI_Over;
	this->type           = DOUBLE;
      }
      that1                = gParse.Nodes + Node1;
      this->value.nelem    = that1->value.nelem;
      this->value.naxis    = that1->value.naxis;
      for( i=0; i < that1->value.naxis; i++ )
	 this->value.naxes[i] = that1->value.naxes[i];
      if (Op == gtiover_fct) {
	this->SubNodes[2]  = Node2;
	that2 = gParse.Nodes + Node2;
	if (that1->value.nelem != that2->value.nelem) {
	  fferror("Dimensions of TIME and TIME_STOP must match for GTIOVERLAP");
	  return(-1);
	}
      }

      /* Init START/STOP node to be treated as a "constant" */

      this->SubNodes[0]    = Node0;
      that0                = gParse.Nodes + Node0;
      that0->operation     = CONST_OP;
      that0->DoOp          = NULL;
      that0->value.data.ptr= NULL;

      /*  Read in START/STOP times  */

      if( ffgkyj( fptr, "NAXIS2", &nrows, NULL, &gParse.status ) )
	 return(-1);
      that0->value.nelem = nrows;
      if( nrows ) {

	 that0->value.data.dblptr = (double*)malloc( 2*nrows*sizeof(double) );
	 if( !that0->value.data.dblptr ) {
	    gParse.status = MEMORY_ALLOCATION;
	    return(-1);
	 }
	 
	 ffgcvd( fptr, startCol, 1L, 1L, nrows, 0.0,
		 that0->value.data.dblptr, &i, &gParse.status );
	 ffgcvd( fptr, stopCol, 1L, 1L, nrows, 0.0,
		 that0->value.data.dblptr+nrows, &i, &gParse.status );
	 if( gParse.status ) {
	    free( that0->value.data.dblptr );
	    return(-1);
	 }

	 /*  Test for fully time-ordered GTI... both START && STOP  */

	 that0->type = 1; /*  Assume yes  */
	 i = nrows;
	 while( --i )
	    if(    that0->value.data.dblptr[i-1]
                   >= that0->value.data.dblptr[i]
		|| that0->value.data.dblptr[i-1+nrows]
		   >= that0->value.data.dblptr[i+nrows] ) {
	       that0->type = 0;
	       break;
	    }

	 /* GTIOVERLAP() requires ordered GTI */
	 if (that0->type != 1 && Op == gtiover_fct) {
	   fferror("Input GTI must be time-ordered for GTIOVERLAP");
	   return(-1);
	 }
	 
	 /*  Handle TIMEZERO offset, if any  */
	 
	 dt = (timeZeroI[1] - timeZeroI[0]) + (timeZeroF[1] - timeZeroF[0]);
	 timeSpan = that0->value.data.dblptr[nrows+nrows-1]
	    - that0->value.data.dblptr[0];
	 
	 if( fabs( dt / timeSpan ) > 1e-12 ) {
	    for( i=0; i<(nrows+nrows); i++ )
	       that0->value.data.dblptr[i] += dt;
	 }
      }
<<<<<<< HEAD
      /* If Node1 is constant (gtifilt_fct) or
	 Node1 and Node2 are constant (gtiover_fct), then evaluate now */
      if( OPER(Node1)==CONST_OP && (Op == gtifilt_fct || OPER(Node2)==CONST_OP)) {
	this->DoOp( this );
      }
=======
      if( OPER(Node1)==CONST_OP )
	 this->DoOp( this );
>>>>>>> 1087ff3af1d00ab4a1ed241a7ccd73ecfb5839a0
   }

   if( samefile )
      ffmahd( fptr, evthdu, &hdutype, &gParse.status );
   else
      ffclos( fptr, &gParse.status );

   return( n );
}

static int New_REG( char *fname, int NodeX, int NodeY, char *colNames )
{
   Node *this, *that0;
   int  type, n, Node0;
   int  Xcol, Ycol, tstat;
   WCSdata wcs;
   SAORegion *Rgn;
   char *cX, *cY;
   FFSTYPE colVal;

   if( NodeX==-99 ) {
      type = ffGetVariable( "X", &colVal );
      if( type==COLUMN ) {
	 NodeX = New_Column( (int)colVal.lng );
      } else {
	 fferror("Could not build X column for REGFILTER");
	 return(-1);
      }
   }
   if( NodeY==-99 ) {
      type = ffGetVariable( "Y", &colVal );
      if( type==COLUMN ) {
	 NodeY = New_Column( (int)colVal.lng );
      } else {
	 fferror("Could not build Y column for REGFILTER");
	 return(-1);
      }
   }
   NodeX = New_Unary( DOUBLE, 0, NodeX );
   NodeY = New_Unary( DOUBLE, 0, NodeY );
   Node0 = Alloc_Node(); /* This will hold the Region Data */
   if( NodeX<0 || NodeY<0 || Node0<0 ) return(-1);

   if( ! (Test_Dims( NodeX, NodeY ) ) ) {
     fferror("Dimensions of REGFILTER arguments are not compatible");
     return (-1);
   }

   n = Alloc_Node();
   if( n >= 0 ) {
      this                 = gParse.Nodes + n;
      this->nSubNodes      = 3;
      this->SubNodes[0]    = Node0;
      this->SubNodes[1]    = NodeX;
      this->SubNodes[2]    = NodeY;
      this->operation      = (int)regfilt_fct;
      this->DoOp           = Do_REG;
      this->type           = BOOLEAN;
      this->value.nelem    = 1;
      this->value.naxis    = 1;
      this->value.naxes[0] = 1;
      
      Copy_Dims(n, NodeX);
      if( SIZE(NodeX)<SIZE(NodeY) )  Copy_Dims(n, NodeY);

      /* Init Region node to be treated as a "constant" */

      that0                = gParse.Nodes + Node0;
      that0->operation     = CONST_OP;
      that0->DoOp          = NULL;

      /*  Identify what columns to use for WCS information  */

      Xcol = Ycol = 0;
      if( *colNames ) {
	 /*  Use the column names in this string for WCS info  */
	 while( *colNames==' ' ) colNames++;
	 cX = cY = colNames;
	 while( *cY && *cY!=' ' && *cY!=',' ) cY++;
	 if( *cY )
	    *(cY++) = '\0';
	 while( *cY==' ' ) cY++;
	 if( !*cY ) {
	    fferror("Could not extract valid pair of column names from REGFILTER");
	    Free_Last_Node();
	    return( -1 );
	 }
	 fits_get_colnum( gParse.def_fptr, CASEINSEN, cX, &Xcol,
			  &gParse.status );
	 fits_get_colnum( gParse.def_fptr, CASEINSEN, cY, &Ycol,
			  &gParse.status );
	 if( gParse.status ) {
	    fferror("Could not locate columns indicated for WCS info");
	    Free_Last_Node();
	    return( -1 );
	 }

      } else {
	 /*  Try to find columns used in X/Y expressions  */
	 Xcol = Locate_Col( gParse.Nodes + NodeX );
	 Ycol = Locate_Col( gParse.Nodes + NodeY );
	 if( Xcol<0 || Ycol<0 ) {
	    fferror("Found multiple X/Y column references in REGFILTER");
	    Free_Last_Node();
	    return( -1 );
	 }
      }

      /*  Now, get the WCS info, if it exists, from the indicated columns  */
      wcs.exists = 0;
      if( Xcol>0 && Ycol>0 ) {
	 tstat = 0;
	 ffgtcs( gParse.def_fptr, Xcol, Ycol,
		 &wcs.xrefval, &wcs.yrefval,
		 &wcs.xrefpix, &wcs.yrefpix,
		 &wcs.xinc,    &wcs.yinc,
		 &wcs.rot,      wcs.type,
		 &tstat );
	 if( tstat==NO_WCS_KEY ) {
	    wcs.exists = 0;
	 } else if( tstat ) {
	    gParse.status = tstat;
	    Free_Last_Node();
	    return( -1 );
	 } else {
	    wcs.exists = 1;
	 }
      }

      /*  Read in Region file  */

      fits_read_rgnfile( fname, &wcs, &Rgn, &gParse.status );
      if( gParse.status ) {
	 Free_Last_Node();
	 return( -1 );
      }

      that0->value.data.ptr = Rgn;

      if( OPER(NodeX)==CONST_OP && OPER(NodeY)==CONST_OP )
	 this->DoOp( this );
   }

   return( n );
}

static int New_Vector( int subNode )
{
   Node *this, *that;
   int n;

   n = Alloc_Node();
   if( n >= 0 ) {
      this              = gParse.Nodes + n;
      that              = gParse.Nodes + subNode;
      this->type        = that->type;
      this->nSubNodes   = 1;
      this->SubNodes[0] = subNode;
      this->operation   = '{';
      this->DoOp        = Do_Vector;
   }

   return( n );
}

static int Close_Vec( int vecNode )
{
   Node *this;
   int n, nelem=0;

   this = gParse.Nodes + vecNode;
   for( n=0; n < this->nSubNodes; n++ ) {
      if( TYPE( this->SubNodes[n] ) != this->type ) {
	 this->SubNodes[n] = New_Unary( this->type, 0, this->SubNodes[n] );
	 if( this->SubNodes[n]<0 ) return(-1);
      }
      nelem += SIZE(this->SubNodes[n]);
   }
   this->value.naxis    = 1;
   this->value.nelem    = nelem;
   this->value.naxes[0] = nelem;

   return( vecNode );
}

static int Locate_Col( Node *this )
/*  Locate the TABLE column number of any columns in "this" calculation.  */
/*  Return ZERO if none found, or negative if more than 1 found.          */
{
   Node *that;
   int  i, col=0, newCol, nfound=0;
   
   if( this->nSubNodes==0
       && this->operation<=0 && this->operation!=CONST_OP )
      return gParse.colData[ - this->operation].colnum;

   for( i=0; i<this->nSubNodes; i++ ) {
      that = gParse.Nodes + this->SubNodes[i];
      if( that->operation>0 ) {
	 newCol = Locate_Col( that );
	 if( newCol<=0 ) {
	    nfound += -newCol;
	 } else {
	    if( !nfound ) {
	       col = newCol;
	       nfound++;
	    } else if( col != newCol ) {
	       nfound++;
	    }
	 }
      } else if( that->operation!=CONST_OP ) {
	 /*  Found a Column  */
	 newCol = gParse.colData[- that->operation].colnum;
	 if( !nfound ) {
	    col = newCol;
	    nfound++;
	 } else if( col != newCol ) {
	    nfound++;
	 }
      }
   }
   if( nfound!=1 )
      return( - nfound );
   else
      return( col );
}

static int Test_Dims( int Node1, int Node2 )
{
   Node *that1, *that2;
   int valid, i;

   if( Node1<0 || Node2<0 ) return(0);

   that1 = gParse.Nodes + Node1;
   that2 = gParse.Nodes + Node2;

   if( that1->value.nelem==1 || that2->value.nelem==1 )
      valid = 1;
   else if( that1->type==that2->type
	    && that1->value.nelem==that2->value.nelem
	    && that1->value.naxis==that2->value.naxis ) {
      valid = 1;
      for( i=0; i<that1->value.naxis; i++ ) {
	 if( that1->value.naxes[i]!=that2->value.naxes[i] )
	    valid = 0;
      }
   } else
      valid = 0;
   return( valid );
}   

static void Copy_Dims( int Node1, int Node2 )
{
   Node *that1, *that2;
   int i;

   if( Node1<0 || Node2<0 ) return;

   that1 = gParse.Nodes + Node1;
   that2 = gParse.Nodes + Node2;

   that1->value.nelem = that2->value.nelem;
   that1->value.naxis = that2->value.naxis;
   for( i=0; i<that2->value.naxis; i++ )
      that1->value.naxes[i] = that2->value.naxes[i];
}

/********************************************************************/
/*    Routines for actually evaluating the expression start here    */
/********************************************************************/

void Evaluate_Parser( long firstRow, long nRows )
    /***********************************************************************/
    /*  Reset the parser for processing another batch of data...           */
    /*    firstRow:  Row number of the first element to evaluate           */
    /*    nRows:     Number of rows to be processed                        */
    /*  Initialize each COLUMN node so that its UNDEF and DATA pointers    */
    /*  point to the appropriate column arrays.                            */
    /*  Finally, call Evaluate_Node for final node.                        */
    /***********************************************************************/
{
   int     i, column;
   long    offset, rowOffset;
   static int rand_initialized = 0;

   /* Initialize the random number generator once and only once */
   if (rand_initialized == 0) {
     simplerng_srand( (unsigned int) time(NULL) );
     rand_initialized = 1;
   }

   gParse.firstRow = firstRow;
   gParse.nRows    = nRows;

   /*  Reset Column Nodes' pointers to point to right data and UNDEF arrays  */

   rowOffset = firstRow - gParse.firstDataRow;
   for( i=0; i<gParse.nNodes; i++ ) {
     if(    OPER(i) >  0 || OPER(i) == CONST_OP ) continue;

      column = -OPER(i);
      offset = gParse.varData[column].nelem * rowOffset;

      gParse.Nodes[i].value.undef = gParse.varData[column].undef + offset;

      switch( gParse.Nodes[i].type ) {
      case BITSTR:
	 gParse.Nodes[i].value.data.strptr =
	    (char**)gParse.varData[column].data + rowOffset;
	 gParse.Nodes[i].value.undef       = NULL;
	 break;
      case STRING:
	 gParse.Nodes[i].value.data.strptr = 
	    (char**)gParse.varData[column].data + rowOffset;
	 gParse.Nodes[i].value.undef = gParse.varData[column].undef + rowOffset;
	 break;
      case BOOLEAN:
	 gParse.Nodes[i].value.data.logptr = 
	    (char*)gParse.varData[column].data + offset;
	 break;
      case LONG:
	 gParse.Nodes[i].value.data.lngptr = 
	    (long*)gParse.varData[column].data + offset;
	 break;
      case DOUBLE:
	 gParse.Nodes[i].value.data.dblptr = 
	    (double*)gParse.varData[column].data + offset;
	 break;
      }
   }

   Evaluate_Node( gParse.resultNode );
}

static void Evaluate_Node( int thisNode )
    /**********************************************************************/
    /*  Recursively evaluate thisNode's subNodes, then call one of the    */
    /*  Do_<Action> functions pointed to by thisNode's DoOp element.      */
    /**********************************************************************/
{
   Node *this;
   int i;
   
   if( gParse.status ) return;

   this = gParse.Nodes + thisNode;
   if( this->operation>0 ) {  /* <=0 indicate constants and columns */
      i = this->nSubNodes;
      while( i-- ) {
	 Evaluate_Node( this->SubNodes[i] );
	 if( gParse.status ) return;
      }
      this->DoOp( this );
   }
}

static void Allocate_Ptrs( Node *this )
{
   long elem, row, size;

   if( this->type==BITSTR || this->type==STRING ) {

      this->value.data.strptr = (char**)malloc( gParse.nRows
						* sizeof(char*) );
      if( this->value.data.strptr ) {
	 this->value.data.strptr[0] = (char*)malloc( gParse.nRows
						     * (this->value.nelem+2)
						     * sizeof(char) );
	 if( this->value.data.strptr[0] ) {
	    row = 0;
	    while( (++row)<gParse.nRows ) {
	       this->value.data.strptr[row] =
		  this->value.data.strptr[row-1] + this->value.nelem+1;
	    }
	    if( this->type==STRING ) {
	       this->value.undef = this->value.data.strptr[row-1]
                                   + this->value.nelem+1;
	    } else {
	       this->value.undef = NULL;  /* BITSTRs don't use undef array */
	    }
	 } else {
	    gParse.status = MEMORY_ALLOCATION;
	    free( this->value.data.strptr );
	 }
      } else {
	 gParse.status = MEMORY_ALLOCATION;
      }

   } else {

      elem = this->value.nelem * gParse.nRows;
      switch( this->type ) {
      case DOUBLE:  size = sizeof( double ); break;
      case LONG:    size = sizeof( long   ); break;
      case BOOLEAN: size = sizeof( char   ); break;
      default:      size = 1;                break;
      }

      this->value.data.ptr = calloc(size+1, elem);

      if( this->value.data.ptr==NULL ) {
	 gParse.status = MEMORY_ALLOCATION;
      } else {
	 this->value.undef = (char *)this->value.data.ptr + elem*size;
      }
   }
}

static void Do_Unary( Node *this )
{
   Node *that;
   long elem;

   that = gParse.Nodes + this->SubNodes[0];

   if( that->operation==CONST_OP ) {  /* Operating on a constant! */
      switch( this->operation ) {
      case DOUBLE:
      case FLTCAST:
	 if( that->type==LONG )
	    this->value.data.dbl = (double)that->value.data.lng;
	 else if( that->type==BOOLEAN )
	    this->value.data.dbl = ( that->value.data.log ? 1.0 : 0.0 );
	 break;
      case LONG:
      case INTCAST:
	 if( that->type==DOUBLE )
	    this->value.data.lng = (long)that->value.data.dbl;
	 else if( that->type==BOOLEAN )
	    this->value.data.lng = ( that->value.data.log ? 1L : 0L );
	 break;
      case BOOLEAN:
	 if( that->type==DOUBLE )
	    this->value.data.log = ( that->value.data.dbl != 0.0 );
	 else if( that->type==LONG )
	    this->value.data.log = ( that->value.data.lng != 0L );
	 break;
      case UMINUS:
	 if( that->type==DOUBLE )
	    this->value.data.dbl = - that->value.data.dbl;
	 else if( that->type==LONG )
	    this->value.data.lng = - that->value.data.lng;
	 break;
      case NOT:
	 if( that->type==BOOLEAN )
	    this->value.data.log = ( ! that->value.data.log );
	 else if( that->type==BITSTR )
	    bitnot( this->value.data.str, that->value.data.str );
	 break;
      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      if( !gParse.status ) {

	 if( this->type!=BITSTR ) {
	    elem = gParse.nRows;
	    if( this->type!=STRING )
	       elem *= this->value.nelem;
	    while( elem-- )
	       this->value.undef[elem] = that->value.undef[elem];
	 }

	 elem = gParse.nRows * this->value.nelem;

	 switch( this->operation ) {

	 case BOOLEAN:
	    if( that->type==DOUBLE )
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( that->value.data.dblptr[elem] != 0.0 );
	    else if( that->type==LONG )
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( that->value.data.lngptr[elem] != 0L );
	    break;

	 case DOUBLE:
	 case FLTCAST:
	    if( that->type==LONG )
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     (double)that->value.data.lngptr[elem];
	    else if( that->type==BOOLEAN )
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     ( that->value.data.logptr[elem] ? 1.0 : 0.0 );
	    break;

	 case LONG:
	 case INTCAST:
	    if( that->type==DOUBLE )
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     (long)that->value.data.dblptr[elem];
	    else if( that->type==BOOLEAN )
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     ( that->value.data.logptr[elem] ? 1L : 0L );
	    break;

	 case UMINUS:
	    if( that->type==DOUBLE ) {
	       while( elem-- )
		  this->value.data.dblptr[elem] =
		     - that->value.data.dblptr[elem];
	    } else if( that->type==LONG ) {
	       while( elem-- )
		  this->value.data.lngptr[elem] =
		     - that->value.data.lngptr[elem];
	    }
	    break;

	 case NOT:
	    if( that->type==BOOLEAN ) {
	       while( elem-- )
		  this->value.data.logptr[elem] =
		     ( ! that->value.data.logptr[elem] );
	    } else if( that->type==BITSTR ) {
	       elem = gParse.nRows;
	       while( elem-- )
		  bitnot( this->value.data.strptr[elem],
			  that->value.data.strptr[elem] );
	    }
	    break;
	 }
      }
   }

   if( that->operation>0 ) {
      free( that->value.data.ptr );
   }
}

static void Do_Offset( Node *this )
{
   Node *col;
   long fRow, nRowOverlap, nRowReload, rowOffset;
   long nelem, elem, offset, nRealElem;
   int status;

   col       = gParse.Nodes + this->SubNodes[0];
   rowOffset = gParse.Nodes[  this->SubNodes[1] ].value.data.lng;

   Allocate_Ptrs( this );

   fRow   = gParse.firstRow + rowOffset;
   if( this->type==STRING || this->type==BITSTR )
      nRealElem = 1;
   else
      nRealElem = this->value.nelem;

   nelem = nRealElem;

   if( fRow < gParse.firstDataRow ) {

      /* Must fill in data at start of array */

      nRowReload = gParse.firstDataRow - fRow;
      if( nRowReload > gParse.nRows ) nRowReload = gParse.nRows;
      nRowOverlap = gParse.nRows - nRowReload;

      offset = 0;

      /*  NULLify any values falling out of bounds  */

      while( fRow<1 && nRowReload>0 ) {
	 if( this->type == BITSTR ) {
	    nelem = this->value.nelem;
	    this->value.data.strptr[offset][ nelem ] = '\0';
	    while( nelem-- ) this->value.data.strptr[offset][nelem] = '0';
	    offset++;
	 } else {
	    while( nelem-- )
	       this->value.undef[offset++] = 1;
	 }
	 nelem = nRealElem;
	 fRow++;
	 nRowReload--;
      }

   } else if( fRow + gParse.nRows > gParse.firstDataRow + gParse.nDataRows ) {

      /* Must fill in data at end of array */

      nRowReload = (fRow+gParse.nRows) - (gParse.firstDataRow+gParse.nDataRows);
      if( nRowReload>gParse.nRows ) {
	 nRowReload = gParse.nRows;
      } else {
	 fRow = gParse.firstDataRow + gParse.nDataRows;
      }
      nRowOverlap = gParse.nRows - nRowReload;

      offset = nRowOverlap * nelem;

      /*  NULLify any values falling out of bounds  */

      elem = gParse.nRows * nelem;
      while( fRow+nRowReload>gParse.totalRows && nRowReload>0 ) {
	 if( this->type == BITSTR ) {
	    nelem = this->value.nelem;
	    elem--;
	    this->value.data.strptr[elem][ nelem ] = '\0';
	    while( nelem-- ) this->value.data.strptr[elem][nelem] = '0';
	 } else {
	    while( nelem-- )
	       this->value.undef[--elem] = 1;
	 }
	 nelem = nRealElem;
	 nRowReload--;
      }

   } else {

      nRowReload  = 0;
      nRowOverlap = gParse.nRows;
      offset      = 0;

   }

   if( nRowReload>0 ) {
      switch( this->type ) {
      case BITSTR:
      case STRING:
	 status = (*gParse.loadData)( -col->operation, fRow, nRowReload,
				      this->value.data.strptr+offset,
				      this->value.undef+offset );
	 break;
      case BOOLEAN:
	 status = (*gParse.loadData)( -col->operation, fRow, nRowReload,
				      this->value.data.logptr+offset,
				      this->value.undef+offset );
	 break;
      case LONG:
	 status = (*gParse.loadData)( -col->operation, fRow, nRowReload,
				      this->value.data.lngptr+offset,
				      this->value.undef+offset );
	 break;
      case DOUBLE:
	 status = (*gParse.loadData)( -col->operation, fRow, nRowReload,
				      this->value.data.dblptr+offset,
				      this->value.undef+offset );
	 break;
      }
   }

   /*  Now copy over the overlapping region, if any  */

   if( nRowOverlap <= 0 ) return;

   if( rowOffset>0 )
      elem = nRowOverlap * nelem;
   else
      elem = gParse.nRows * nelem;

   offset = nelem * rowOffset;
   while( nRowOverlap-- && !gParse.status ) {
      while( nelem-- && !gParse.status ) {
	 elem--;
	 if( this->type != BITSTR )
	    this->value.undef[elem] = col->value.undef[elem+offset];
	 switch( this->type ) {
	 case BITSTR:
	    strcpy( this->value.data.strptr[elem       ],
                     col->value.data.strptr[elem+offset] );
	    break;
	 case STRING:
	    strcpy( this->value.data.strptr[elem       ],
                     col->value.data.strptr[elem+offset] );
	    break;
	 case BOOLEAN:
	    this->value.data.logptr[elem] = col->value.data.logptr[elem+offset];
	    break;
	 case LONG:
	    this->value.data.lngptr[elem] = col->value.data.lngptr[elem+offset];
	    break;
	 case DOUBLE:
	    this->value.data.dblptr[elem] = col->value.data.dblptr[elem+offset];
	    break;
	 }
      }
      nelem = nRealElem;
   }
}

static void Do_BinOp_bit( Node *this )
{
   Node *that1, *that2;
   char *sptr1=NULL, *sptr2=NULL;
   int  const1, const2;
   long rows;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   const1 = ( that1->operation==CONST_OP );
   const2 = ( that2->operation==CONST_OP );
   sptr1  = ( const1 ? that1->value.data.str : NULL );
   sptr2  = ( const2 ? that2->value.data.str : NULL );

   if( const1 && const2 ) {
      switch( this->operation ) {
      case NE:
	 this->value.data.log = !bitcmp( sptr1, sptr2 );
	 break;
      case EQ:
	 this->value.data.log =  bitcmp( sptr1, sptr2 );
	 break;
      case GT:
      case LT:
      case LTE:
      case GTE:
	 this->value.data.log = bitlgte( sptr1, this->operation, sptr2 );
	 break;
      case '|': 
	 bitor( this->value.data.str, sptr1, sptr2 );
	 break;
      case '&': 
	 bitand( this->value.data.str, sptr1, sptr2 );
	 break;
      case '+':
	 strcpy( this->value.data.str, sptr1 );
	 strcat( this->value.data.str, sptr2 );
	 break;
      case ACCUM:
	this->value.data.lng = 0;
	while( *sptr1 ) {
	  if ( *sptr1 == '1' ) this->value.data.lng ++;
	  sptr1 ++;
	}
	break;
	
      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      if( !gParse.status ) {
	 rows  = gParse.nRows;
	 switch( this->operation ) {

	    /*  BITSTR comparisons  */

	 case NE:
	 case EQ:
	 case GT:
	 case LT:
	 case LTE:
	 case GTE:
	    while( rows-- ) {
	       if( !const1 )
		  sptr1 = that1->value.data.strptr[rows];
	       if( !const2 )
		  sptr2 = that2->value.data.strptr[rows];
	       switch( this->operation ) {
	       case NE:  this->value.data.logptr[rows] = 
                                                      !bitcmp( sptr1, sptr2 );
                         break;
	       case EQ:  this->value.data.logptr[rows] = 
                                                       bitcmp( sptr1, sptr2 );
                         break;
	       case GT:
	       case LT:
	       case LTE:
	       case GTE: this->value.data.logptr[rows] = 
                                     bitlgte( sptr1, this->operation, sptr2 );
	                 break;
	       }
	       this->value.undef[rows] = 0;
	    }
	    break;
	 
	    /*  BITSTR AND/ORs ...  no UNDEFS in or out */
      
	 case '|': 
	 case '&': 
	 case '+':
	    while( rows-- ) {
	       if( !const1 )
		  sptr1 = that1->value.data.strptr[rows];
	       if( !const2 )
		  sptr2 = that2->value.data.strptr[rows];
	       if( this->operation=='|' )
		  bitor(  this->value.data.strptr[rows], sptr1, sptr2 );
	       else if( this->operation=='&' )
		  bitand( this->value.data.strptr[rows], sptr1, sptr2 );
	       else {
		  strcpy( this->value.data.strptr[rows], sptr1 );
		  strcat( this->value.data.strptr[rows], sptr2 );
	       }
	    }
	    break;

	    /* Accumulate 1 bits */
	 case ACCUM:
	   { 
	     long i, previous, curr;

	     previous = that2->value.data.lng;
	     
	      /* Cumulative sum of this chunk */
	     for (i=0; i<rows; i++) {
	       sptr1 = that1->value.data.strptr[i];
	       for (curr = 0; *sptr1; sptr1 ++) {
		 if ( *sptr1 == '1' ) curr ++;
	       }
	       previous += curr;
	       this->value.data.lngptr[i] = previous;
	       this->value.undef[i] = 0;
	     }
	     
	      /* Store final cumulant for next pass */
	     that2->value.data.lng = previous;
	   }
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.strptr[0] );
      free( that1->value.data.strptr    );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.strptr[0] );
      free( that2->value.data.strptr    );
   }
}

static void Do_BinOp_str( Node *this )
{
   Node *that1, *that2;
   char *sptr1, *sptr2, null1=0, null2=0;
   int const1, const2, val;
   long rows;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   const1 = ( that1->operation==CONST_OP );
   const2 = ( that2->operation==CONST_OP );
   sptr1  = ( const1 ? that1->value.data.str : NULL );
   sptr2  = ( const2 ? that2->value.data.str : NULL );

   if( const1 && const2 ) {  /*  Result is a constant  */
      switch( this->operation ) {

	 /*  Compare Strings  */

      case NE:
      case EQ:
	 val = ( FSTRCMP( sptr1, sptr2 ) == 0 );
	 this->value.data.log = ( this->operation==EQ ? val : !val );
	 break;
      case GT:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) > 0 );
	 break;
      case LT:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) < 0 );
	 break;
      case GTE:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) >= 0 );
	 break;
      case LTE:
	 this->value.data.log = ( FSTRCMP( sptr1, sptr2 ) <= 0 );
	 break;

	 /*  Concat Strings  */

      case '+':
	 strcpy( this->value.data.str, sptr1 );
	 strcat( this->value.data.str, sptr2 );
	 break;
      }
      this->operation = CONST_OP;

   } else {  /*  Not a constant  */

      Allocate_Ptrs( this );

      if( !gParse.status ) {

	 rows = gParse.nRows;
	 switch( this->operation ) {

	    /*  Compare Strings  */

	 case NE:
	 case EQ:
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  val = ( FSTRCMP( sptr1, sptr2 ) == 0 );
		  this->value.data.logptr[rows] =
		     ( this->operation==EQ ? val : !val );
	       }
	    }
	    break;
	    
	 case GT:
	 case LT:
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  val = ( FSTRCMP( sptr1, sptr2 ) );
		  this->value.data.logptr[rows] =
		     ( this->operation==GT ? val>0 : val<0 );
	       }
	    }
	    break;

	 case GTE:
	 case LTE:
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  val = ( FSTRCMP( sptr1, sptr2 ) );
		  this->value.data.logptr[rows] =
		     ( this->operation==GTE ? val>=0 : val<=0 );
	       }
	    }
	    break;

	    /*  Concat Strings  */
	    
	 case '+':
	    while( rows-- ) {
	       if( !const1 ) null1 = that1->value.undef[rows];
	       if( !const2 ) null2 = that2->value.undef[rows];
	       this->value.undef[rows] = (null1 || null2);
	       if( ! this->value.undef[rows] ) {
		  if( !const1 ) sptr1  = that1->value.data.strptr[rows];
		  if( !const2 ) sptr2  = that2->value.data.strptr[rows];
		  strcpy( this->value.data.strptr[rows], sptr1 );
		  strcat( this->value.data.strptr[rows], sptr2 );
	       }
	    }
	    break;
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.strptr[0] );
      free( that1->value.data.strptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.strptr[0] );
      free( that2->value.data.strptr );
   }
}

static void Do_BinOp_log( Node *this )
{
   Node *that1, *that2;
   int vector1, vector2;
   char val1=0, val2=0, null1=0, null2=0;
   long rows, nelem, elem;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.log;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.log;
   }

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */
      switch( this->operation ) {
      case OR:
	 this->value.data.log = (val1 || val2);
	 break;
      case AND:
	 this->value.data.log = (val1 && val2);
	 break;
      case EQ:
	 this->value.data.log = ( (val1 && val2) || (!val1 && !val2) );
	 break;
      case NE:
	 this->value.data.log = ( (val1 && !val2) || (!val1 && val2) );
	 break;
      case ACCUM:
	 this->value.data.lng = val1;
	 break;
      }
      this->operation=CONST_OP;
   } else if (this->operation == ACCUM) {
      long i, previous, curr;
      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;
      
      Allocate_Ptrs( this );
      
      if( !gParse.status ) {
	previous = that2->value.data.lng;
	
	/* Cumulative sum of this chunk */
	for (i=0; i<elem; i++) {
	  if (!that1->value.undef[i]) {
	    curr = that1->value.data.logptr[i];
	    previous += curr;
	  }
	  this->value.data.lngptr[i] = previous;
	  this->value.undef[i] = 0;
	}
	
	/* Store final cumulant for next pass */
	that2->value.data.lng = previous;
      }
      
   } else {
      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( this );

      if( !gParse.status ) {
	
	 if (this->operation == ACCUM) {
	   long i, previous, curr;
	   
	   previous = that2->value.data.lng;
	   
	   /* Cumulative sum of this chunk */
	   for (i=0; i<elem; i++) {
	     if (!that1->value.undef[i]) {
	       curr = that1->value.data.logptr[i];
	       previous += curr;
	     }
	     this->value.data.lngptr[i] = previous;
	     this->value.undef[i] = 0;
	   }
	   
	   /* Store final cumulant for next pass */
	   that2->value.data.lng = previous;
	 }
	
	 while( rows-- ) {
	    while( nelem-- ) {
	       elem--;

	       if( vector1>1 ) {
		  val1  = that1->value.data.logptr[elem];
		  null1 = that1->value.undef[elem];
	       } else if( vector1 ) {
		  val1  = that1->value.data.logptr[rows];
		  null1 = that1->value.undef[rows];
	       }

	       if( vector2>1 ) {
		  val2  = that2->value.data.logptr[elem];
		  null2 = that2->value.undef[elem];
	       } else if( vector2 ) {
		  val2  = that2->value.data.logptr[rows];
		  null2 = that2->value.undef[rows];
	       }

	       this->value.undef[elem] = (null1 || null2);
	       switch( this->operation ) {

	       case OR:
		  /*  This is more complicated than others to suppress UNDEFs */
		  /*  in those cases where the other argument is DEF && TRUE  */

		  if( !null1 && !null2 ) {
		     this->value.data.logptr[elem] = (val1 || val2);
		  } else if( (null1 && !null2 && val2)
			     || ( !null1 && null2 && val1 ) ) {
		     this->value.data.logptr[elem] = 1;
		     this->value.undef[elem] = 0;
		  }
		  break;

	       case AND:
		  /*  This is more complicated than others to suppress UNDEFs */
		  /*  in those cases where the other argument is DEF && FALSE */

		  if( !null1 && !null2 ) {
		     this->value.data.logptr[elem] = (val1 && val2);
		  } else if( (null1 && !null2 && !val2)
			     || ( !null1 && null2 && !val1 ) ) {
		     this->value.data.logptr[elem] = 0;
		     this->value.undef[elem] = 0;
		  }
		  break;

	       case EQ:
		  this->value.data.logptr[elem] = 
		     ( (val1 && val2) || (!val1 && !val2) );
		  break;

	       case NE:
		  this->value.data.logptr[elem] =
		     ( (val1 && !val2) || (!val1 && val2) );
		  break;
	       }
	    }
	    nelem = this->value.nelem;
	 }
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

static void Do_BinOp_lng( Node *this )
{
   Node *that1, *that2;
   int  vector1, vector2;
   long val1=0, val2=0;
   char null1=0, null2=0;
   long rows, nelem, elem;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.lng;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.lng;
   }

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */

      switch( this->operation ) {
      case '~':   /* Treat as == for LONGS */
      case EQ:    this->value.data.log = (val1 == val2);   break;
      case NE:    this->value.data.log = (val1 != val2);   break;
      case GT:    this->value.data.log = (val1 >  val2);   break;
      case LT:    this->value.data.log = (val1 <  val2);   break;
      case LTE:   this->value.data.log = (val1 <= val2);   break;
      case GTE:   this->value.data.log = (val1 >= val2);   break;

      case '+':   this->value.data.lng = (val1  + val2);   break;
      case '-':   this->value.data.lng = (val1  - val2);   break;
      case '*':   this->value.data.lng = (val1  * val2);   break;

      case '&':   this->value.data.lng = (val1  & val2);   break;
      case '|':   this->value.data.lng = (val1  | val2);   break;
      case '^':   this->value.data.lng = (val1  ^ val2);   break;

      case '%':
	 if( val2 ) this->value.data.lng = (val1 % val2);
	 else       fferror("Divide by Zero");
	 break;
      case '/': 
	 if( val2 ) this->value.data.lng = (val1 / val2); 
	 else       fferror("Divide by Zero");
	 break;
      case POWER:
	 this->value.data.lng = (long)pow((double)val1,(double)val2);
	 break;
      case ACCUM:
	 this->value.data.lng = val1;
	 break;
      case DIFF:
	 this->value.data.lng = 0;
	 break;
      }
      this->operation=CONST_OP;

   } else if ((this->operation == ACCUM) || (this->operation == DIFF)) {
      long i, previous, curr;
      long undef;
      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;
      
      Allocate_Ptrs( this );
      
      if( !gParse.status ) {
	previous = that2->value.data.lng;
	undef    = (long) that2->value.undef;
	
	if (this->operation == ACCUM) {
	  /* Cumulative sum of this chunk */
	  for (i=0; i<elem; i++) {
	    if (!that1->value.undef[i]) {
	      curr = that1->value.data.lngptr[i];
	      previous += curr;
	    }
	    this->value.data.lngptr[i] = previous;
	    this->value.undef[i] = 0;
	  }
	} else {
	  /* Sequential difference for this chunk */
	  for (i=0; i<elem; i++) {
	    curr = that1->value.data.lngptr[i];
	    if (that1->value.undef[i] || undef) {
	      /* Either this, or previous, value was undefined */
	      this->value.data.lngptr[i] = 0;
	      this->value.undef[i] = 1;
	    } else {
	      /* Both defined, we are okay! */
	      this->value.data.lngptr[i] = curr - previous;
	      this->value.undef[i] = 0;
	    }

	    previous = curr;
	    undef = that1->value.undef[i];
	  }
	}	  
	
	/* Store final cumulant for next pass */
	that2->value.data.lng = previous;
	that2->value.undef    = (char *) undef; /* XXX evil, but no harm here */
      }
      
   } else {

      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( this );

      while( rows-- && !gParse.status ) {
	 while( nelem-- && !gParse.status ) {
	    elem--;

	    if( vector1>1 ) {
	       val1  = that1->value.data.lngptr[elem];
	       null1 = that1->value.undef[elem];
	    } else if( vector1 ) {
	       val1  = that1->value.data.lngptr[rows];
	       null1 = that1->value.undef[rows];
	    }

	    if( vector2>1 ) {
	       val2  = that2->value.data.lngptr[elem];
	       null2 = that2->value.undef[elem];
	    } else if( vector2 ) {
	       val2  = that2->value.data.lngptr[rows];
	       null2 = that2->value.undef[rows];
	    }

	    this->value.undef[elem] = (null1 || null2);
	    switch( this->operation ) {
	    case '~':   /* Treat as == for LONGS */
	    case EQ:   this->value.data.logptr[elem] = (val1 == val2);   break;
	    case NE:   this->value.data.logptr[elem] = (val1 != val2);   break;
	    case GT:   this->value.data.logptr[elem] = (val1 >  val2);   break;
	    case LT:   this->value.data.logptr[elem] = (val1 <  val2);   break;
	    case LTE:  this->value.data.logptr[elem] = (val1 <= val2);   break;
	    case GTE:  this->value.data.logptr[elem] = (val1 >= val2);   break;
	       
	    case '+':  this->value.data.lngptr[elem] = (val1  + val2);   break;
	    case '-':  this->value.data.lngptr[elem] = (val1  - val2);   break;
	    case '*':  this->value.data.lngptr[elem] = (val1  * val2);   break;

	    case '&':  this->value.data.lngptr[elem] = (val1  & val2);   break;
	    case '|':  this->value.data.lngptr[elem] = (val1  | val2);   break;
	    case '^':  this->value.data.lngptr[elem] = (val1  ^ val2);   break;

	    case '%':   
	       if( val2 ) this->value.data.lngptr[elem] = (val1 % val2);
	       else {
		 this->value.data.lngptr[elem] = 0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case '/': 
	       if( val2 ) this->value.data.lngptr[elem] = (val1 / val2); 
	       else {
		 this->value.data.lngptr[elem] = 0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case POWER:
	       this->value.data.lngptr[elem] = (long)pow((double)val1,(double)val2);
	       break;
	    }
	 }
	 nelem = this->value.nelem;
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

static void Do_BinOp_dbl( Node *this )
{
   Node   *that1, *that2;
   int    vector1, vector2;
   double val1=0.0, val2=0.0;
   char   null1=0, null2=0;
   long   rows, nelem, elem;

   that1 = gParse.Nodes + this->SubNodes[0];
   that2 = gParse.Nodes + this->SubNodes[1];

   vector1 = ( that1->operation!=CONST_OP );
   if( vector1 )
      vector1 = that1->value.nelem;
   else {
      val1  = that1->value.data.dbl;
   }

   vector2 = ( that2->operation!=CONST_OP );
   if( vector2 )
      vector2 = that2->value.nelem;
   else {
      val2  = that2->value.data.dbl;
   } 

   if( !vector1 && !vector2 ) {  /*  Result is a constant  */

      switch( this->operation ) {
      case '~':   this->value.data.log = ( fabs(val1-val2) < APPROX );   break;
      case EQ:    this->value.data.log = (val1 == val2);   break;
      case NE:    this->value.data.log = (val1 != val2);   break;
      case GT:    this->value.data.log = (val1 >  val2);   break;
      case LT:    this->value.data.log = (val1 <  val2);   break;
      case LTE:   this->value.data.log = (val1 <= val2);   break;
      case GTE:   this->value.data.log = (val1 >= val2);   break;

      case '+':   this->value.data.dbl = (val1  + val2);   break;
      case '-':   this->value.data.dbl = (val1  - val2);   break;
      case '*':   this->value.data.dbl = (val1  * val2);   break;

      case '%':
	 if( val2 ) this->value.data.dbl = val1 - val2*((int)(val1/val2));
	 else       fferror("Divide by Zero");
	 break;
      case '/': 
	 if( val2 ) this->value.data.dbl = (val1 / val2); 
	 else       fferror("Divide by Zero");
	 break;
      case POWER:
	 this->value.data.dbl = (double)pow(val1,val2);
	 break;
      case ACCUM:
	 this->value.data.dbl = val1;
	 break;
      case DIFF:
	this->value.data.dbl = 0;
	 break;
      }
      this->operation=CONST_OP;

   } else if ((this->operation == ACCUM) || (this->operation == DIFF)) {
      long i;
      long undef;
      double previous, curr;
      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;
      
      Allocate_Ptrs( this );
      
      if( !gParse.status ) {
	previous = that2->value.data.dbl;
	undef    = (long) that2->value.undef;
	
	if (this->operation == ACCUM) {
	  /* Cumulative sum of this chunk */
	  for (i=0; i<elem; i++) {
	    if (!that1->value.undef[i]) {
	      curr = that1->value.data.dblptr[i];
	      previous += curr;
	    }
	    this->value.data.dblptr[i] = previous;
	    this->value.undef[i] = 0;
	  }
	} else {
	  /* Sequential difference for this chunk */
	  for (i=0; i<elem; i++) {
	    curr = that1->value.data.dblptr[i];
	    if (that1->value.undef[i] || undef) {
	      /* Either this, or previous, value was undefined */
	      this->value.data.dblptr[i] = 0;
	      this->value.undef[i] = 1;
	    } else {
	      /* Both defined, we are okay! */
	      this->value.data.dblptr[i] = curr - previous;
	      this->value.undef[i] = 0;
	    }

	    previous = curr;
	    undef = that1->value.undef[i];
	  }
	}	  
	
	/* Store final cumulant for next pass */
	that2->value.data.dbl = previous;
	that2->value.undef    = (char *) undef; /* XXX evil, but no harm here */
      }
      
   } else {

      rows  = gParse.nRows;
      nelem = this->value.nelem;
      elem  = this->value.nelem * rows;

      Allocate_Ptrs( this );

      while( rows-- && !gParse.status ) {
	 while( nelem-- && !gParse.status ) {
	    elem--;

	    if( vector1>1 ) {
	       val1  = that1->value.data.dblptr[elem];
	       null1 = that1->value.undef[elem];
	    } else if( vector1 ) {
	       val1  = that1->value.data.dblptr[rows];
	       null1 = that1->value.undef[rows];
	    }

	    if( vector2>1 ) {
	       val2  = that2->value.data.dblptr[elem];
	       null2 = that2->value.undef[elem];
	    } else if( vector2 ) {
	       val2  = that2->value.data.dblptr[rows];
	       null2 = that2->value.undef[rows];
	    }

	    this->value.undef[elem] = (null1 || null2);
	    switch( this->operation ) {
	    case '~':   this->value.data.logptr[elem] =
                                          ( fabs(val1-val2) < APPROX );   break;
	    case EQ:    this->value.data.logptr[elem] = (val1 == val2);   break;
	    case NE:    this->value.data.logptr[elem] = (val1 != val2);   break;
	    case GT:    this->value.data.logptr[elem] = (val1 >  val2);   break;
	    case LT:    this->value.data.logptr[elem] = (val1 <  val2);   break;
	    case LTE:   this->value.data.logptr[elem] = (val1 <= val2);   break;
	    case GTE:   this->value.data.logptr[elem] = (val1 >= val2);   break;
	       
	    case '+':   this->value.data.dblptr[elem] = (val1  + val2);   break;
	    case '-':   this->value.data.dblptr[elem] = (val1  - val2);   break;
	    case '*':   this->value.data.dblptr[elem] = (val1  * val2);   break;

	    case '%':
	       if( val2 ) this->value.data.dblptr[elem] =
                                val1 - val2*((int)(val1/val2));
	       else {
		 this->value.data.dblptr[elem] = 0.0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case '/': 
	       if( val2 ) this->value.data.dblptr[elem] = (val1 / val2); 
	       else {
		 this->value.data.dblptr[elem] = 0.0;
		 this->value.undef[elem] = 1;
	       }
	       break;
	    case POWER:
	       this->value.data.dblptr[elem] = (double)pow(val1,val2);
	       break;
	    }
	 }
	 nelem = this->value.nelem;
      }
   }

   if( that1->operation>0 ) {
      free( that1->value.data.ptr );
   }
   if( that2->operation>0 ) {
      free( that2->value.data.ptr );
   }
}

/*
 *  This Quickselect routine is based on the algorithm described in
 *  "Numerical recipes in C", Second Edition,
 *  Cambridge University Press, 1992, Section 8.5, ISBN 0-521-43108-5
 *  This code by Nicolas Devillard - 1998. Public domain.
 * http://ndevilla.free.fr/median/median/src/quickselect.c
 */

#define ELEM_SWAP(a,b) { register long t=(a);(a)=(b);(b)=t; }

/* 
 * qselect_median_lng - select the median value of a long array
 *
 * This routine selects the median value of the long integer array
 * arr[].  If there are an even number of elements, the "lower median"
 * is selected.
 *
 * The array arr[] is scrambled, so users must operate on a scratch
 * array if they wish the values to be preserved.
 *
 * long arr[] - array of values
 * int n - number of elements in arr
 *
 * RETURNS: the lower median value of arr[]
 *
 */
long qselect_median_lng(long arr[], int n)
{
    int low, high ;
    int median;
    int middle, ll, hh;

    low = 0 ; high = n-1 ; median = (low + high) / 2;
    for (;;) {

        if (high <= low) { /* One element only */
	  return arr[median];	  
	}

        if (high == low + 1) {  /* Two elements only */
            if (arr[low] > arr[high])
                ELEM_SWAP(arr[low], arr[high]) ;
	    return arr[median];
        }

    /* Find median of low, middle and high items; swap into position low */
    middle = (low + high) / 2;
    if (arr[middle] > arr[high])    ELEM_SWAP(arr[middle], arr[high]) ;
    if (arr[low] > arr[high])       ELEM_SWAP(arr[low], arr[high]) ;
    if (arr[middle] > arr[low])     ELEM_SWAP(arr[middle], arr[low]) ;

    /* Swap low item (now in position middle) into position (low+1) */
    ELEM_SWAP(arr[middle], arr[low+1]) ;

    /* Nibble from each end towards middle, swapping items when stuck */
    ll = low + 1;
    hh = high;
    for (;;) {
        do ll++; while (arr[low] > arr[ll]) ;
        do hh--; while (arr[hh]  > arr[low]) ;

        if (hh < ll)
        break;

        ELEM_SWAP(arr[ll], arr[hh]) ;
    }

    /* Swap middle item (in position low) back into correct position */
    ELEM_SWAP(arr[low], arr[hh]) ;

    /* Re-set active partition */
    if (hh <= median)
        low = ll;
        if (hh >= median)
        high = hh - 1;
    }
}

#undef ELEM_SWAP

#define ELEM_SWAP(a,b) { register double t=(a);(a)=(b);(b)=t; }

/* 
 * qselect_median_dbl - select the median value of a double array
 *
 * This routine selects the median value of the double array
 * arr[].  If there are an even number of elements, the "lower median"
 * is selected.
 *
 * The array arr[] is scrambled, so users must operate on a scratch
 * array if they wish the values to be preserved.
 *
 * double arr[] - array of values
 * int n - number of elements in arr
 *
 * RETURNS: the lower median value of arr[]
 *
 */
double qselect_median_dbl(double arr[], int n)
{
    int low, high ;
    int median;
    int middle, ll, hh;

    low = 0 ; high = n-1 ; median = (low + high) / 2;
    for (;;) {
        if (high <= low) { /* One element only */
            return arr[median] ;
	}

        if (high == low + 1) {  /* Two elements only */
            if (arr[low] > arr[high])
                ELEM_SWAP(arr[low], arr[high]) ;
            return arr[median] ;
        }

    /* Find median of low, middle and high items; swap into position low */
    middle = (low + high) / 2;
    if (arr[middle] > arr[high])    ELEM_SWAP(arr[middle], arr[high]) ;
    if (arr[low] > arr[high])       ELEM_SWAP(arr[low], arr[high]) ;
    if (arr[middle] > arr[low])     ELEM_SWAP(arr[middle], arr[low]) ;

    /* Swap low item (now in position middle) into position (low+1) */
    ELEM_SWAP(arr[middle], arr[low+1]) ;

    /* Nibble from each end towards middle, swapping items when stuck */
    ll = low + 1;
    hh = high;
    for (;;) {
        do ll++; while (arr[low] > arr[ll]) ;
        do hh--; while (arr[hh]  > arr[low]) ;

        if (hh < ll)
        break;

        ELEM_SWAP(arr[ll], arr[hh]) ;
    }

    /* Swap middle item (in position low) back into correct position */
    ELEM_SWAP(arr[low], arr[hh]) ;

    /* Re-set active partition */
    if (hh <= median)
        low = ll;
        if (hh >= median)
        high = hh - 1;
    }
}

#undef ELEM_SWAP

/*
 * angsep_calc - compute angular separation between celestial coordinates
 *   
 * This routine computes the angular separation between to coordinates
 * on the celestial sphere (i.e. RA and Dec).  Note that all units are
 * in DEGREES, unlike the other trig functions in the calculator.
 *
 * double ra1, dec1 - RA and Dec of the first position in degrees
 * double ra2, dec2 - RA and Dec of the second position in degrees
 * 
 * RETURNS: (double) angular separation in degrees
 *
 */
double angsep_calc(double ra1, double dec1, double ra2, double dec2)
{
/*  double cd;  */
  static double deg = 0;
  double a, sdec, sra;
  
  if (deg == 0) deg = ((double)4)*atan((double)1)/((double)180);
  /* deg = 1.0; **** UNCOMMENT IF YOU WANT RADIANS */

  /* The algorithm is the law of Haversines.  This algorithm is
     stable even when the points are close together.  The normal
     Law of Cosines fails for angles around 0.1 arcsec. */

  sra  = sin( (ra2 - ra1)*deg / 2 );
  sdec = sin( (dec2 - dec1)*deg / 2);
  a = sdec*sdec + cos(dec1*deg)*cos(dec2*deg)*sra*sra;

  /* Sanity checking to avoid a range error in the sqrt()'s below */
  if (a < 0) { a = 0; }
  if (a > 1) { a = 1; }

  return 2.0*atan2(sqrt(a), sqrt(1.0 - a)) / deg;
}

static void Do_Func( Node *this )
{
   Node *theParams[MAXSUBS];
   int  vector[MAXSUBS], allConst;
   lval pVals[MAXSUBS];
   char pNull[MAXSUBS];
   long   ival;
   double dval;
   int  i, valInit;
   long row, elem, nelem;

   i = this->nSubNodes;
   allConst = 1;
   while( i-- ) {
      theParams[i] = gParse.Nodes + this->SubNodes[i];
      vector[i]   = ( theParams[i]->operation!=CONST_OP );
      if( vector[i] ) {
	 allConst = 0;
	 vector[i] = theParams[i]->value.nelem;
      } else {
	 if( theParams[i]->type==DOUBLE ) {
	    pVals[i].data.dbl = theParams[i]->value.data.dbl;
	 } else if( theParams[i]->type==LONG ) {
	    pVals[i].data.lng = theParams[i]->value.data.lng;
	 } else if( theParams[i]->type==BOOLEAN ) {
	    pVals[i].data.log = theParams[i]->value.data.log;
	 } else
	    strcpy(pVals[i].data.str, theParams[i]->value.data.str);
	 pNull[i] = 0;
      }
   }

   if( this->nSubNodes==0 ) allConst = 0; /* These do produce scalars */
   /* Random numbers are *never* constant !! */
   if( this->operation == poirnd_fct ) allConst = 0;
   if( this->operation == gasrnd_fct ) allConst = 0;
   if( this->operation == rnd_fct ) allConst = 0;

   if( allConst ) {

      switch( this->operation ) {

	    /* Non-Trig single-argument functions */

	 case sum_fct:
	    if( theParams[0]->type==BOOLEAN )
	       this->value.data.lng = ( pVals[0].data.log ? 1 : 0 );
	    else if( theParams[0]->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else if( theParams[0]->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    else if( theParams[0]->type==BITSTR )
	      strcpy(this->value.data.str, pVals[0].data.str);
	    break;
         case average_fct:
	    if( theParams[0]->type==LONG )
	       this->value.data.dbl = pVals[0].data.lng;
	    else if( theParams[0]->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    break;
         case stddev_fct:
	    this->value.data.dbl = 0;  /* Standard deviation of a constant = 0 */
	    break;
	 case median_fct:
	    if( theParams[0]->type==BOOLEAN )
	       this->value.data.lng = ( pVals[0].data.log ? 1 : 0 );
	    else if( theParams[0]->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else
	       this->value.data.dbl = pVals[0].data.dbl;
	    break;

	 case poirnd_fct:
	    if( theParams[0]->type==DOUBLE )
	      this->value.data.lng = simplerng_getpoisson(pVals[0].data.dbl);
	    else
	      this->value.data.lng = simplerng_getpoisson(pVals[0].data.lng);
	    break;

	 case abs_fct:
	    if( theParams[0]->type==DOUBLE ) {
	       dval = pVals[0].data.dbl;
	       this->value.data.dbl = (dval>0.0 ? dval : -dval);
	    } else {
	       ival = pVals[0].data.lng;
	       this->value.data.lng = (ival> 0  ? ival : -ival);
	    }
	    break;

            /* Special Null-Handling Functions */

         case nonnull_fct:
	    this->value.data.lng = 1; /* Constants are always 1-element and defined */
	    break;
         case isnull_fct:  /* Constants are always defined */
	    this->value.data.log = 0;
	    break;
         case defnull_fct:
	    if( this->type==BOOLEAN )
	       this->value.data.log = pVals[0].data.log;
            else if( this->type==LONG )
	       this->value.data.lng = pVals[0].data.lng;
            else if( this->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
            else if( this->type==STRING )
	       strcpy(this->value.data.str,pVals[0].data.str);
	    break;
        case setnull_fct: /* Only defined for numeric expressions */
            if( this->type==LONG )
 	      this->value.data.lng = pVals[0].data.lng;
            else if( this->type==DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    break;

	    /* Math functions with 1 double argument */

	 case sin_fct:
	    this->value.data.dbl = sin( pVals[0].data.dbl );
	    break;
	 case cos_fct:
	    this->value.data.dbl = cos( pVals[0].data.dbl );
	    break;
	 case tan_fct:
	    this->value.data.dbl = tan( pVals[0].data.dbl );
	    break;
	 case asin_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<-1.0 || dval>1.0 )
	       fferror("Out of range argument to arcsin");
	    else
	       this->value.data.dbl = asin( dval );
	    break;
	 case acos_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<-1.0 || dval>1.0 )
	       fferror("Out of range argument to arccos");
	    else
	       this->value.data.dbl = acos( dval );
	    break;
	 case atan_fct:
	    this->value.data.dbl = atan( pVals[0].data.dbl );
	    break;
	 case sinh_fct:
	    this->value.data.dbl = sinh( pVals[0].data.dbl );
	    break;
	 case cosh_fct:
	    this->value.data.dbl = cosh( pVals[0].data.dbl );
	    break;
	 case tanh_fct:
	    this->value.data.dbl = tanh( pVals[0].data.dbl );
	    break;
	 case exp_fct:
	    this->value.data.dbl = exp( pVals[0].data.dbl );
	    break;
	 case log_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<=0.0 )
	       fferror("Out of range argument to log");
	    else
	       this->value.data.dbl = log( dval );
	    break;
	 case log10_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<=0.0 )
	       fferror("Out of range argument to log10");
	    else
	       this->value.data.dbl = log10( dval );
	    break;
	 case sqrt_fct:
	    dval = pVals[0].data.dbl;
	    if( dval<0.0 )
	       fferror("Out of range argument to sqrt");
	    else
	       this->value.data.dbl = sqrt( dval );
	    break;
	 case ceil_fct:
	    this->value.data.dbl = ceil( pVals[0].data.dbl );
	    break;
	 case floor_fct:
	    this->value.data.dbl = floor( pVals[0].data.dbl );
	    break;
	 case round_fct:
	    this->value.data.dbl = floor( pVals[0].data.dbl + 0.5 );
	    break;

	    /* Two-argument Trig Functions */

	 case atan2_fct:
	    this->value.data.dbl =
	       atan2( pVals[0].data.dbl, pVals[1].data.dbl );
	    break;

	    /* Four-argument ANGSEP function */
         case angsep_fct:
	    this->value.data.dbl = 
	      angsep_calc(pVals[0].data.dbl, pVals[1].data.dbl,
			  pVals[2].data.dbl, pVals[3].data.dbl);

	    /*  Min/Max functions taking 1 or 2 arguments  */

         case min1_fct:
	    /* No constant vectors! */
	    if( this->type == DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    else if( this->type == LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else if( this->type == BITSTR )
	      strcpy(this->value.data.str, pVals[0].data.str);
	    break;
         case min2_fct:
	    if( this->type == DOUBLE )
	       this->value.data.dbl =
		  minvalue( pVals[0].data.dbl, pVals[1].data.dbl );
	    else if( this->type == LONG )
	       this->value.data.lng =
		  minvalue( pVals[0].data.lng, pVals[1].data.lng );
	    break;
         case max1_fct:
	    /* No constant vectors! */
	    if( this->type == DOUBLE )
	       this->value.data.dbl = pVals[0].data.dbl;
	    else if( this->type == LONG )
	       this->value.data.lng = pVals[0].data.lng;
	    else if( this->type == BITSTR )
	      strcpy(this->value.data.str, pVals[0].data.str);
	    break;
         case max2_fct:
	    if( this->type == DOUBLE )
	       this->value.data.dbl =
		  maxvalue( pVals[0].data.dbl, pVals[1].data.dbl );
	    else if( this->type == LONG )
	       this->value.data.lng =
		  maxvalue( pVals[0].data.lng, pVals[1].data.lng );
	    break;

	    /* Boolean SAO region Functions... scalar or vector dbls */

	 case near_fct:
	    this->value.data.log = bnear( pVals[0].data.dbl, pVals[1].data.dbl,
					  pVals[2].data.dbl );
	    break;
	 case circle_fct:
	    this->value.data.log = circle( pVals[0].data.dbl, pVals[1].data.dbl,
					   pVals[2].data.dbl, pVals[3].data.dbl,
					   pVals[4].data.dbl );
	    break;
	 case box_fct:
	    this->value.data.log = saobox( pVals[0].data.dbl, pVals[1].data.dbl,
					   pVals[2].data.dbl, pVals[3].data.dbl,
					   pVals[4].data.dbl, pVals[5].data.dbl,
					   pVals[6].data.dbl );
	    break;
	 case elps_fct:
	    this->value.data.log =
                               ellipse( pVals[0].data.dbl, pVals[1].data.dbl,
					pVals[2].data.dbl, pVals[3].data.dbl,
					pVals[4].data.dbl, pVals[5].data.dbl,
					pVals[6].data.dbl );
	    break;

            /* C Conditional expression:  bool ? expr : expr */

         case ifthenelse_fct:
            switch( this->type ) {
            case BOOLEAN:
               this->value.data.log = ( pVals[2].data.log ?
                                        pVals[0].data.log : pVals[1].data.log );
               break;
            case LONG:
               this->value.data.lng = ( pVals[2].data.log ?
                                        pVals[0].data.lng : pVals[1].data.lng );
               break;
            case DOUBLE:
               this->value.data.dbl = ( pVals[2].data.log ?
                                        pVals[0].data.dbl : pVals[1].data.dbl );
               break;
            case STRING:
	       strcpy(this->value.data.str, ( pVals[2].data.log ?
                                              pVals[0].data.str :
                                              pVals[1].data.str ) );
               break;
            }
            break;

	    /* String functions */
         case strmid_fct:
	   cstrmid(this->value.data.str, this->value.nelem, 
		   pVals[0].data.str,    pVals[0].nelem,
		   pVals[1].data.lng);
	   break;
         case strpos_fct:
	   {
	     char *res = strstr(pVals[0].data.str, pVals[1].data.str);
	     if (res == NULL) {
	       this->value.data.lng = 0; 
	     } else {
	       this->value.data.lng = (res - pVals[0].data.str) + 1;
	     }
	     break;
	   }

      }
      this->operation = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      row  = gParse.nRows;
      elem = row * this->value.nelem;

      if( !gParse.status ) {
	 switch( this->operation ) {

	    /* Special functions with no arguments */

	 case row_fct:
	    while( row-- ) {
	       this->value.data.lngptr[row] = gParse.firstRow + row;
	       this->value.undef[row] = 0;
	    }
	    break;
	 case null_fct:
            if( this->type==LONG ) {
               while( row-- ) {
                  this->value.data.lngptr[row] = 0;
                  this->value.undef[row] = 1;
               }
            } else if( this->type==STRING ) {
               while( row-- ) {
                  this->value.data.strptr[row][0] = '\0';
                  this->value.undef[row] = 1;
               }
            }
	    break;
	 case rnd_fct:
	   while( elem-- ) {
	     this->value.data.dblptr[elem] = simplerng_getuniform();
	     this->value.undef[elem] = 0;
	    }
	    break;

	 case gasrnd_fct:
	    while( elem-- ) {
	       this->value.data.dblptr[elem] = simplerng_getnorm();
	       this->value.undef[elem] = 0;
	    }
	    break;

	 case poirnd_fct:
	   if( theParams[0]->type==DOUBLE ) {
	      if (theParams[0]->operation == CONST_OP) {
		while( elem-- ) {
		  this->value.undef[elem] = (pVals[0].data.dbl < 0);
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = simplerng_getpoisson(pVals[0].data.dbl);
		  }
		} 
	      } else {
		while( elem-- ) {
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		  if (theParams[0]->value.data.dblptr[elem] < 0) 
		    this->value.undef[elem] = 1;
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = 
		      simplerng_getpoisson(theParams[0]->value.data.dblptr[elem]);
		  }
		} /* while */
	      } /* ! CONST_OP */
	   } else {
	     /* LONG */
	      if (theParams[0]->operation == CONST_OP) {
		while( elem-- ) {
		  this->value.undef[elem] = (pVals[0].data.lng < 0);
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = simplerng_getpoisson(pVals[0].data.lng);
		  }
		} 
	      } else {
		while( elem-- ) {
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		  if (theParams[0]->value.data.lngptr[elem] < 0) 
		    this->value.undef[elem] = 1;
		  if (! this->value.undef[elem]) {
		    this->value.data.lngptr[elem] = 
		      simplerng_getpoisson(theParams[0]->value.data.lngptr[elem]);
		  }
		} /* while */
	      } /* ! CONST_OP */
	   } /* END LONG */
	   break;


	    /* Non-Trig single-argument functions */
	    
	 case sum_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( theParams[0]->type==BOOLEAN ) {
	       while( row-- ) {
		  this->value.data.lngptr[row] = 0;
		  /* Default is UNDEF until a defined value is found */
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( ! theParams[0]->value.undef[elem] ) {
		       this->value.data.lngptr[row] +=
			 ( theParams[0]->value.data.logptr[elem] ? 1 : 0 );
		       this->value.undef[row] = 0;
		     }
		  }
	       }
	    } else if( theParams[0]->type==LONG ) {
	       while( row-- ) {
		  this->value.data.lngptr[row] = 0;
		  /* Default is UNDEF until a defined value is found */
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( ! theParams[0]->value.undef[elem] ) {
		       this->value.data.lngptr[row] +=
			 theParams[0]->value.data.lngptr[elem];
		       this->value.undef[row] = 0;
		     }
		  }
	       }		  
	    } else if( theParams[0]->type==DOUBLE ){
	       while( row-- ) {
		  this->value.data.dblptr[row] = 0.0;
		  /* Default is UNDEF until a defined value is found */
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( ! theParams[0]->value.undef[elem] ) {
		       this->value.data.dblptr[row] +=
			 theParams[0]->value.data.dblptr[elem];
		       this->value.undef[row] = 0;
		     }
		  }
	       }		  
	    } else { /* BITSTR */
	       nelem = theParams[0]->value.nelem;
	       while( row-- ) {
		  char *sptr1 = theParams[0]->value.data.strptr[row];
		  this->value.data.lngptr[row] = 0;
		  this->value.undef[row] = 0;
		  while (*sptr1) {
		    if (*sptr1 == '1') this->value.data.lngptr[row] ++;
		    sptr1++;
		  }
	       }		  
	    }
	    break;

	 case average_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( theParams[0]->type==LONG ) {
	       while( row-- ) {
		  int count = 0;
		  this->value.data.dblptr[row] = 0;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       this->value.data.dblptr[row] +=
			 theParams[0]->value.data.lngptr[elem];
		       count ++;
		     }
		  }
		  if (count == 0) {
		    this->value.undef[row] = 1;
		  } else {
		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] /= count;
		  }
	       }		  
	    } else if( theParams[0]->type==DOUBLE ){
	       while( row-- ) {
		  int count = 0;
		  this->value.data.dblptr[row] = 0;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       this->value.data.dblptr[row] +=
			 theParams[0]->value.data.dblptr[elem];
		       count ++;
		     }
		  }
		  if (count == 0) {
		    this->value.undef[row] = 1;
		  } else {
		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] /= count;
		  }
	       }		  
	    }
	    break;
	 case stddev_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( theParams[0]->type==LONG ) {

	       /* Compute the mean value */
	       while( row-- ) {
		  int count = 0;
		  double sum = 0, sum2 = 0;

		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       sum += theParams[0]->value.data.lngptr[elem];
		       count ++;
		     }
		  }
		  if (count > 1) {
		    sum /= count;

		    /* Compute the sum of squared deviations */
		    nelem = theParams[0]->value.nelem;
		    elem += nelem;  /* Reset elem for second pass */
		    while( nelem-- ) {
		      elem--;
		      if (theParams[0]->value.undef[elem] == 0) {
			double dx = (theParams[0]->value.data.lngptr[elem] - sum);
			sum2 += (dx*dx);
		      }
		    }

		    sum2 /= (double)count-1;

		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] = sqrt(sum2);
		  } else {
		    this->value.undef[row] = 0;       /* STDDEV => 0 */
		    this->value.data.dblptr[row] = 0;
		  }
	       }
	    } else if( theParams[0]->type==DOUBLE ){

	       /* Compute the mean value */
	       while( row-- ) {
		  int count = 0;
		  double sum = 0, sum2 = 0;

		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if (theParams[0]->value.undef[elem] == 0) {
		       sum += theParams[0]->value.data.dblptr[elem];
		       count ++;
		     }
		  }
		  if (count > 1) {
		    sum /= count;

		    /* Compute the sum of squared deviations */
		    nelem = theParams[0]->value.nelem;
		    elem += nelem;  /* Reset elem for second pass */
		    while( nelem-- ) {
		      elem--;
		      if (theParams[0]->value.undef[elem] == 0) {
			double dx = (theParams[0]->value.data.dblptr[elem] - sum);
			sum2 += (dx*dx);
		      }
		    }

		    sum2 /= (double)count-1;

		    this->value.undef[row] = 0;
		    this->value.data.dblptr[row] = sqrt(sum2);
		  } else {
		    this->value.undef[row] = 0;       /* STDDEV => 0 */
		    this->value.data.dblptr[row] = 0;
		  }
	       }
	    }
	    break;

	 case median_fct:
	   elem = row * theParams[0]->value.nelem;
	   nelem = theParams[0]->value.nelem;
	   if( theParams[0]->type==LONG ) {
	       long *dptr = theParams[0]->value.data.lngptr;
	       char *uptr = theParams[0]->value.undef;
	       long *mptr = (long *) malloc(sizeof(long)*nelem);
	       int irow;

	       /* Allocate temporary storage for this row, since the
                  quickselect function will scramble the contents */
	       if (mptr == 0) {
		 fferror("Could not allocate temporary memory in median function");
		 free( this->value.data.ptr );
		 break;
	       }

	       for (irow=0; irow<row; irow++) {
		  long *p = mptr;
		  int nelem1 = nelem;


		  while ( nelem1-- ) { 
		    if (*uptr == 0) {
		      *p++ = *dptr;   /* Only advance the dest pointer if we copied */
		    }
		    dptr ++;  /* Advance the source pointer ... */
		    uptr ++;  /* ... and source "undef" pointer */
		  }
		  
		  nelem1 = (p - mptr);  /* Number of accepted data points */
		  if (nelem1 > 0) {
		    this->value.undef[irow] = 0;
		    this->value.data.lngptr[irow] = qselect_median_lng(mptr, nelem1);
		  } else {
		    this->value.undef[irow] = 1;
		    this->value.data.lngptr[irow] = 0;
		  }
		    
	       }		  

	       free(mptr);
	    } else {
	       double *dptr = theParams[0]->value.data.dblptr;
	       char   *uptr = theParams[0]->value.undef;
	       double *mptr = (double *) malloc(sizeof(double)*nelem);
	       int irow;

	       /* Allocate temporary storage for this row, since the
                  quickselect function will scramble the contents */
	       if (mptr == 0) {
		 fferror("Could not allocate temporary memory in median function");
		 free( this->value.data.ptr );
		 break;
	       }

	       for (irow=0; irow<row; irow++) {
		  double *p = mptr;
		  int nelem1 = nelem;

		  while ( nelem1-- ) { 
		    if (*uptr == 0) {
		      *p++ = *dptr;   /* Only advance the dest pointer if we copied */
		    }
		    dptr ++;  /* Advance the source pointer ... */
		    uptr ++;  /* ... and source "undef" pointer */
		  }

		  nelem1 = (p - mptr);  /* Number of accepted data points */
		  if (nelem1 > 0) {
		    this->value.undef[irow] = 0;
		    this->value.data.dblptr[irow] = qselect_median_dbl(mptr, nelem1);
		  } else {
		    this->value.undef[irow] = 1;
		    this->value.data.dblptr[irow] = 0;
		  }

	       }
	       free(mptr);
	    }
	    break;
	 case abs_fct:
	    if( theParams[0]->type==DOUBLE )
	       while( elem-- ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = (dval>0.0 ? dval : -dval);
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
	       }
	    else
	       while( elem-- ) {
		  ival = theParams[0]->value.data.lngptr[elem];
		  this->value.data.lngptr[elem] = (ival> 0  ? ival : -ival);
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
	       }
	    break;

            /* Special Null-Handling Functions */

	 case nonnull_fct:
	   nelem = theParams[0]->value.nelem;
	   if ( theParams[0]->type==STRING ) nelem = 1;
	   elem = row * nelem;
	   while( row-- ) {
	     int nelem1 = nelem;

	     this->value.undef[row] = 0;        /* Initialize to 0 (defined) */
	     this->value.data.lngptr[row] = 0;
	     while( nelem1-- ) {	
	       elem --;
	       if ( theParams[0]->value.undef[elem] == 0 ) this->value.data.lngptr[row] ++;
	     }
	   }
	   break;
	 case isnull_fct:
	    if( theParams[0]->type==STRING ) elem = row;
	    while( elem-- ) {
	       this->value.data.logptr[elem] = theParams[0]->value.undef[elem];
	       this->value.undef[elem] = 0;
	    }
	    break;
         case defnull_fct:
	    switch( this->type ) {
	    case BOOLEAN:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.logptr[elem] = pVals[1].data.log;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.logptr[elem] = pVals[0].data.log;
		     }
		  }
	       }
	       break;
	    case LONG:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.lngptr[elem] = pVals[1].data.lng;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.lngptr[elem] = pVals[0].data.lng;
		     }
		  }
	       }
	       break;
	    case DOUBLE:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pNull[i] = theParams[i]->value.undef[elem];
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			} else if( vector[i] ) {
			   pNull[i] = theParams[i]->value.undef[row];
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			}
		     if( pNull[0] ) {
			this->value.undef[elem] = pNull[1];
			this->value.data.dblptr[elem] = pVals[1].data.dbl;
		     } else {
			this->value.undef[elem] = 0;
			this->value.data.dblptr[elem] = pVals[0].data.dbl;
		     }
		  }
	       }
	       break;
	    case STRING:
	       while( row-- ) {
		  i=2; while( i-- )
		     if( vector[i] ) {
			pNull[i] = theParams[i]->value.undef[row];
			strcpy(pVals[i].data.str,
			       theParams[i]->value.data.strptr[row]);
		     }
		  if( pNull[0] ) {
		     this->value.undef[row] = pNull[1];
		     strcpy(this->value.data.strptr[row],pVals[1].data.str);
		  } else {
		     this->value.undef[elem] = 0;
		     strcpy(this->value.data.strptr[row],pVals[0].data.str);
		  }
	       }
	    }
	    break;
         case setnull_fct:
	    switch( this->type ) {
	    case LONG:
	      while( elem-- ) {
		if ( theParams[1]->value.data.lng == 
		     theParams[0]->value.data.lngptr[elem] ) {
		  this->value.data.lngptr[elem] = 0;
		  this->value.undef[elem] = 1;
		} else {
		  this->value.data.lngptr[elem] = theParams[0]->value.data.lngptr[elem];
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		}
	      }
	      break;
	    case DOUBLE:
	      while( elem-- ) {
		if ( theParams[1]->value.data.dbl == 
		     theParams[0]->value.data.dblptr[elem] ) {
		  this->value.data.dblptr[elem] = 0;
		  this->value.undef[elem] = 1;
		} else {
		  this->value.data.dblptr[elem] = theParams[0]->value.data.dblptr[elem];
		  this->value.undef[elem] = theParams[0]->value.undef[elem];
		}
	      }
	      break;
	    }
	    break;

	    /* Math functions with 1 double argument */

	 case sin_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     sin( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case cos_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     cos( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case tan_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     tan( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case asin_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<-1.0 || dval>1.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = asin( dval );
	       }
	    break;
	 case acos_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<-1.0 || dval>1.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = acos( dval );
	       }
	    break;
	 case atan_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = atan( dval );
	       }
	    break;
	 case sinh_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     sinh( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case cosh_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     cosh( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case tanh_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     tanh( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case exp_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  this->value.data.dblptr[elem] = exp( dval );
	       }
	    break;
	 case log_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<=0.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = log( dval );
	       }
	    break;
	 case log10_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<=0.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = log10( dval );
	       }
	    break;
	 case sqrt_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  dval = theParams[0]->value.data.dblptr[elem];
		  if( dval<0.0 ) {
		     this->value.data.dblptr[elem] = 0.0;
		     this->value.undef[elem] = 1;
		  } else
		     this->value.data.dblptr[elem] = sqrt( dval );
	       }
	    break;
	 case ceil_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     ceil( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case floor_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     floor( theParams[0]->value.data.dblptr[elem] );
	       }
	    break;
	 case round_fct:
	    while( elem-- )
	       if( !(this->value.undef[elem] = theParams[0]->value.undef[elem]) ) {
		  this->value.data.dblptr[elem] = 
		     floor( theParams[0]->value.data.dblptr[elem] + 0.5);
	       }
	    break;

	    /* Two-argument Trig Functions */
	    
	 case atan2_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=2; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1]) ) )
		     this->value.data.dblptr[elem] =
			atan2( pVals[0].data.dbl, pVals[1].data.dbl );
	       }
	    }
	    break;

	    /* Four-argument ANGSEP Function */
	    
	 case angsep_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=4; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3]) ) )
		     this->value.data.dblptr[elem] =
		       angsep_calc(pVals[0].data.dbl, pVals[1].data.dbl,
				   pVals[2].data.dbl, pVals[3].data.dbl);
	       }
	    }
	    break;



	    /*  Min/Max functions taking 1 or 2 arguments  */

         case min1_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( this->type==LONG ) {
	       long minVal=0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 minVal  = theParams[0]->value.data.lngptr[elem];
		       } else {
			 minVal  = minvalue( minVal,
					     theParams[0]->value.data.lngptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }  
		  this->value.data.lngptr[row] = minVal;
	       }		  
	    } else if( this->type==DOUBLE ) {
	       double minVal=0.0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 minVal  = theParams[0]->value.data.dblptr[elem];
		       } else {
			 minVal  = minvalue( minVal,
					     theParams[0]->value.data.dblptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }  
		  this->value.data.dblptr[row] = minVal;
	       }		  
	    } else if( this->type==BITSTR ) {
	       char minVal;
	       while( row-- ) {
		  char *sptr1 = theParams[0]->value.data.strptr[row];
		  minVal = '1';
		  while (*sptr1) {
		    if (*sptr1 == '0') minVal = '0';
		    sptr1++;
		  }
		  this->value.data.strptr[row][0] = minVal;
		  this->value.data.strptr[row][1] = 0;     /* Null terminate */
	       }		  
	    }
	    break;
         case min2_fct:
	    if( this->type==LONG ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.lngptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[1].data.lng;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[0].data.lng;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] =
			 minvalue( pVals[0].data.lng, pVals[1].data.lng );
		     }
		  }
	       }
	    } else if( this->type==DOUBLE ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.dblptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[1].data.dbl;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[0].data.dbl;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] =
			 minvalue( pVals[0].data.dbl, pVals[1].data.dbl );
		     }
		  }
 	       }
	    }
	    break;

         case max1_fct:
	    elem = row * theParams[0]->value.nelem;
	    if( this->type==LONG ) {
	       long maxVal=0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 maxVal  = theParams[0]->value.data.lngptr[elem];
		       } else {
			 maxVal  = maxvalue( maxVal,
					     theParams[0]->value.data.lngptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }
		  this->value.data.lngptr[row] = maxVal;
	       }		  
	    } else if( this->type==DOUBLE ) {
	       double maxVal=0.0;
	       while( row-- ) {
		  valInit = 1;
		  this->value.undef[row] = 1;
		  nelem = theParams[0]->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     if ( !theParams[0]->value.undef[elem] ) {
		       if ( valInit ) {
			 valInit = 0;
			 maxVal  = theParams[0]->value.data.dblptr[elem];
		       } else {
			 maxVal  = maxvalue( maxVal,
					     theParams[0]->value.data.dblptr[elem] );
		       }
		       this->value.undef[row] = 0;
		     }
		  }
		  this->value.data.dblptr[row] = maxVal;
	       }		  
	    } else if( this->type==BITSTR ) {
	       char maxVal;
	       while( row-- ) {
		  char *sptr1 = theParams[0]->value.data.strptr[row];
		  maxVal = '0';
		  while (*sptr1) {
		    if (*sptr1 == '1') maxVal = '1';
		    sptr1++;
		  }
		  this->value.data.strptr[row][0] = maxVal;
		  this->value.data.strptr[row][1] = 0;     /* Null terminate */
	       }		  
	    }
	    break;
         case max2_fct:
	    if( this->type==LONG ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.lngptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[1].data.lng;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] = pVals[0].data.lng;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.lngptr[elem] =
			 maxvalue( pVals[0].data.lng, pVals[1].data.lng );
		     }
		  }
	       }
	    } else if( this->type==DOUBLE ) {
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( pNull[0] && pNull[1] ) {
		       this->value.undef[elem] = 1;
		       this->value.data.dblptr[elem] = 0;
		     } else if (pNull[0]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[1].data.dbl;
		     } else if (pNull[1]) {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] = pVals[0].data.dbl;
		     } else {
		       this->value.undef[elem] = 0;
		       this->value.data.dblptr[elem] =
			 maxvalue( pVals[0].data.dbl, pVals[1].data.dbl );
		     }
		  }
	       }
	    }
	    break;

	    /* Boolean SAO region Functions... scalar or vector dbls */

	 case near_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=3; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2]) ) )
		    this->value.data.logptr[elem] =
		      bnear( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl );
	       }
	    }
	    break;

	 case circle_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=5; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3] ||
						   pNull[4]) ) )
		    this->value.data.logptr[elem] =
		     circle( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl, pVals[3].data.dbl,
			     pVals[4].data.dbl );
	       }
	    }
	    break;

	 case box_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=7; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3] ||
						   pNull[4] || pNull[5] ||
						   pNull[6] ) ) )
		    this->value.data.logptr[elem] =
		     saobox( pVals[0].data.dbl, pVals[1].data.dbl,
			     pVals[2].data.dbl, pVals[3].data.dbl,
			     pVals[4].data.dbl, pVals[5].data.dbl,
			     pVals[6].data.dbl );	
	       }
	    }
	    break;

	 case elps_fct:
	    while( row-- ) {
	       nelem = this->value.nelem;
	       while( nelem-- ) {
		  elem--;
		  i=7; while( i-- )
		     if( vector[i]>1 ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[elem];
			pNull[i] = theParams[i]->value.undef[elem];
		     } else if( vector[i] ) {
			pVals[i].data.dbl =
			   theParams[i]->value.data.dblptr[row];
			pNull[i] = theParams[i]->value.undef[row];
		     }
		  if( !(this->value.undef[elem] = (pNull[0] || pNull[1] ||
						   pNull[2] || pNull[3] ||
						   pNull[4] || pNull[5] ||
						   pNull[6] ) ) )
		    this->value.data.logptr[elem] =
		     ellipse( pVals[0].data.dbl, pVals[1].data.dbl,
			      pVals[2].data.dbl, pVals[3].data.dbl,
			      pVals[4].data.dbl, pVals[5].data.dbl,
			      pVals[6].data.dbl );
	       }
	    }
	    break;

            /* C Conditional expression:  bool ? expr : expr */

         case ifthenelse_fct:
            switch( this->type ) {
            case BOOLEAN:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
                     if( vector[2]>1 ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[elem];
                        pNull[2] = theParams[2]->value.undef[elem];
                     } else if( vector[2] ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[row];
                        pNull[2] = theParams[2]->value.undef[row];
                     }
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.log =
			      theParams[i]->value.data.logptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( !(this->value.undef[elem] = pNull[2]) ) {
                        if( pVals[2].data.log ) {
                           this->value.data.logptr[elem] = pVals[0].data.log;
                           this->value.undef[elem]       = pNull[0];
                        } else {
                           this->value.data.logptr[elem] = pVals[1].data.log;
                           this->value.undef[elem]       = pNull[1];
                        }
                     }
		  }
	       }
               break;
            case LONG:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
                     if( vector[2]>1 ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[elem];
                        pNull[2] = theParams[2]->value.undef[elem];
                     } else if( vector[2] ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[row];
                        pNull[2] = theParams[2]->value.undef[row];
                     }
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.lng =
			      theParams[i]->value.data.lngptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( !(this->value.undef[elem] = pNull[2]) ) {
                        if( pVals[2].data.log ) {
                           this->value.data.lngptr[elem] = pVals[0].data.lng;
                           this->value.undef[elem]       = pNull[0];
                        } else {
                           this->value.data.lngptr[elem] = pVals[1].data.lng;
                           this->value.undef[elem]       = pNull[1];
                        }
                     }
		  }
	       }
               break;
            case DOUBLE:
	       while( row-- ) {
		  nelem = this->value.nelem;
		  while( nelem-- ) {
		     elem--;
                     if( vector[2]>1 ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[elem];
                        pNull[2] = theParams[2]->value.undef[elem];
                     } else if( vector[2] ) {
                        pVals[2].data.log =
                           theParams[2]->value.data.logptr[row];
                        pNull[2] = theParams[2]->value.undef[row];
                     }
		     i=2; while( i-- )
			if( vector[i]>1 ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[elem];
			   pNull[i] = theParams[i]->value.undef[elem];
			} else if( vector[i] ) {
			   pVals[i].data.dbl =
			      theParams[i]->value.data.dblptr[row];
			   pNull[i] = theParams[i]->value.undef[row];
			}
		     if( !(this->value.undef[elem] = pNull[2]) ) {
                        if( pVals[2].data.log ) {
                           this->value.data.dblptr[elem] = pVals[0].data.dbl;
                           this->value.undef[elem]       = pNull[0];
                        } else {
                           this->value.data.dblptr[elem] = pVals[1].data.dbl;
                           this->value.undef[elem]       = pNull[1];
                        }
                     }
		  }
	       }
               break;
            case STRING:
	       while( row-- ) {
                  if( vector[2] ) {
                     pVals[2].data.log = theParams[2]->value.data.logptr[row];
                     pNull[2] = theParams[2]->value.undef[row];
                  }
                  i=2; while( i-- )
                     if( vector[i] ) {
                        strcpy( pVals[i].data.str,
                                theParams[i]->value.data.strptr[row] );
                        pNull[i] = theParams[i]->value.undef[row];
                     }
                  if( !(this->value.undef[row] = pNull[2]) ) {
                     if( pVals[2].data.log ) {
                        strcpy( this->value.data.strptr[row],
                                pVals[0].data.str );
                        this->value.undef[row]       = pNull[0];
                     } else {
                        strcpy( this->value.data.strptr[row],
                                pVals[1].data.str );
                        this->value.undef[row]       = pNull[1];
                     }
                  } else {
                     this->value.data.strptr[row][0] = '\0';
                  }
	       }
               break;

            }
            break;

	    /* String functions */
            case strmid_fct:
	      {
		int strconst = theParams[0]->operation == CONST_OP;
		int posconst = theParams[1]->operation == CONST_OP;
		int lenconst = theParams[2]->operation == CONST_OP;
		int dest_len = this->value.nelem;
		int src_len  = theParams[0]->value.nelem;

		while (row--) {
		  int pos;
		  int len;
		  char *str;
		  int undef = 0;

		  if (posconst) {
		    pos = theParams[1]->value.data.lng;
		  } else {
		    pos = theParams[1]->value.data.lngptr[row];
		    if (theParams[1]->value.undef[row]) undef = 1;
		  }
		  if (strconst) {
		    str = theParams[0]->value.data.str;
		    if (src_len == 0) src_len = strlen(str);
		  } else {
		    str = theParams[0]->value.data.strptr[row];
		    if (theParams[0]->value.undef[row]) undef = 1;
		  }
		  if (lenconst) {
		    len = dest_len;
		  } else {
		    len = theParams[2]->value.data.lngptr[row];
		    if (theParams[2]->value.undef[row]) undef = 1;
		  }
		  this->value.data.strptr[row][0] = '\0';
		  if (pos == 0) undef = 1;
		  if (! undef ) {
		    if (cstrmid(this->value.data.strptr[row], len,
				str, src_len, pos) < 0) break;
		  }
		  this->value.undef[row] = undef;
		}
	      }		      
	      break;

	    /* String functions */
            case strpos_fct:
	      {
		int const1 = theParams[0]->operation == CONST_OP;
		int const2 = theParams[1]->operation == CONST_OP;

		while (row--) {
		  char *str1, *str2;
		  int undef = 0;

		  if (const1) {
		    str1 = theParams[0]->value.data.str;
		  } else {
		    str1 = theParams[0]->value.data.strptr[row];
		    if (theParams[0]->value.undef[row]) undef = 1;
		  }
		  if (const2) {
		    str2 = theParams[1]->value.data.str;
		  } else {
		    str2 = theParams[1]->value.data.strptr[row];
		    if (theParams[1]->value.undef[row]) undef = 1;
		  }
		  this->value.data.lngptr[row] = 0;
		  if (! undef ) {
		    char *res = strstr(str1, str2);
		    if (res == NULL) {
		      undef = 1;
		      this->value.data.lngptr[row] = 0; 
		    } else {
		      this->value.data.lngptr[row] = (res - str1) + 1;
		    }
		  }
		  this->value.undef[row] = undef;
		}
	      }
	      break;

		    
	 } /* End switch(this->operation) */
      } /* End if (!gParse.status) */
   } /* End non-constant operations */

   i = this->nSubNodes;
   while( i-- ) {
      if( theParams[i]->operation>0 ) {
	 /*  Currently only numeric params allowed  */
	 free( theParams[i]->value.data.ptr );
      }
   }
}

static void Do_Deref( Node *this )
{
   Node *theVar, *theDims[MAXDIMS];
   int  isConst[MAXDIMS], allConst;
   long dimVals[MAXDIMS];
   int  i, nDims;
   long row, elem, dsize;

   theVar = gParse.Nodes + this->SubNodes[0];

   i = nDims = this->nSubNodes-1;
   allConst = 1;
   while( i-- ) {
      theDims[i] = gParse.Nodes + this->SubNodes[i+1];
      isConst[i] = ( theDims[i]->operation==CONST_OP );
      if( isConst[i] )
	 dimVals[i] = theDims[i]->value.data.lng;
      else
	 allConst = 0;
   }

   if( this->type==DOUBLE ) {
      dsize = sizeof( double );
   } else if( this->type==LONG ) {
      dsize = sizeof( long );
   } else if( this->type==BOOLEAN ) {
      dsize = sizeof( char );
   } else
      dsize = 0;

   Allocate_Ptrs( this );

   if( !gParse.status ) {

      if( allConst && theVar->value.naxis==nDims ) {

	 /* Dereference completely using constant indices */

	 elem = 0;
	 i    = nDims;
	 while( i-- ) {
	    if( dimVals[i]<1 || dimVals[i]>theVar->value.naxes[i] ) break;
	    elem = theVar->value.naxes[i]*elem + dimVals[i]-1;
	 }
	 if( i<0 ) {
	    for( row=0; row<gParse.nRows; row++ ) {
	       if( this->type==STRING )
		 this->value.undef[row] = theVar->value.undef[row];
	       else if( this->type==BITSTR ) 
		 this->value.undef;  /* Dummy - BITSTRs do not have undefs */
	       else 
		 this->value.undef[row] = theVar->value.undef[elem];

	       if( this->type==DOUBLE )
		  this->value.data.dblptr[row] = 
		     theVar->value.data.dblptr[elem];
	       else if( this->type==LONG )
		  this->value.data.lngptr[row] = 
		     theVar->value.data.lngptr[elem];
	       else if( this->type==BOOLEAN )
		  this->value.data.logptr[row] = 
		     theVar->value.data.logptr[elem];
	       else {
		 /* XXX Note, the below expression uses knowledge of
                    the layout of the string format, namely (nelem+1)
                    characters per string, followed by (nelem+1)
                    "undef" values. */
		  this->value.data.strptr[row][0] = 
		     theVar->value.data.strptr[0][elem+row];
		  this->value.data.strptr[row][1] = 0;  /* Null terminate */
	       }
	       elem += theVar->value.nelem;
	    }
	 } else {
	    fferror("Index out of range");
	    free( this->value.data.ptr );
	 }
	 
      } else if( allConst && nDims==1 ) {
	 
	 /* Reduce dimensions by 1, using a constant index */
	 
	 if( dimVals[0] < 1 ||
	     dimVals[0] > theVar->value.naxes[ theVar->value.naxis-1 ] ) {
	    fferror("Index out of range");
	    free( this->value.data.ptr );
	 } else if ( this->type == BITSTR || this->type == STRING ) {
	    elem = this->value.nelem * (dimVals[0]-1);
	    for( row=0; row<gParse.nRows; row++ ) {
	      if (this->value.undef) 
		this->value.undef[row] = theVar->value.undef[row];
	      memcpy( (char*)this->value.data.strptr[0]
		      + row*sizeof(char)*(this->value.nelem+1),
		      (char*)theVar->value.data.strptr[0] + elem*sizeof(char),
		      this->value.nelem * sizeof(char) );
	      /* Null terminate */
	      this->value.data.strptr[row][this->value.nelem] = 0;
	      elem += theVar->value.nelem+1;
	    }	       
	 } else {
	    elem = this->value.nelem * (dimVals[0]-1);
	    for( row=0; row<gParse.nRows; row++ ) {
	       memcpy( this->value.undef + row*this->value.nelem,
		       theVar->value.undef + elem,
		       this->value.nelem * sizeof(char) );
	       memcpy( (char*)this->value.data.ptr
		       + row*dsize*this->value.nelem,
		       (char*)theVar->value.data.ptr + elem*dsize,
		       this->value.nelem * dsize );
	       elem += theVar->value.nelem;
	    }	       
	 }
      
      } else if( theVar->value.naxis==nDims ) {

	 /* Dereference completely using an expression for the indices */

	 for( row=0; row<gParse.nRows; row++ ) {

	    for( i=0; i<nDims; i++ ) {
	       if( !isConst[i] ) {
		  if( theDims[i]->value.undef[row] ) {
		     fferror("Null encountered as vector index");
		     free( this->value.data.ptr );
		     break;
		  } else
		     dimVals[i] = theDims[i]->value.data.lngptr[row];
	       }
	    }
	    if( gParse.status ) break;

	    elem = 0;
	    i    = nDims;
	    while( i-- ) {
	       if( dimVals[i]<1 || dimVals[i]>theVar->value.naxes[i] ) break;
	       elem = theVar->value.naxes[i]*elem + dimVals[i]-1;
	    }
	    if( i<0 ) {
	       elem += row*theVar->value.nelem;

	       if( this->type==STRING )
		 this->value.undef[row] = theVar->value.undef[row];
	       else if( this->type==BITSTR ) 
		 this->value.undef;  /* Dummy - BITSTRs do not have undefs */
	       else 
		 this->value.undef[row] = theVar->value.undef[elem];

	       if( this->type==DOUBLE )
		  this->value.data.dblptr[row] = 
		     theVar->value.data.dblptr[elem];
	       else if( this->type==LONG )
		  this->value.data.lngptr[row] = 
		     theVar->value.data.lngptr[elem];
	       else if( this->type==BOOLEAN )
		  this->value.data.logptr[row] = 
		     theVar->value.data.logptr[elem];
	       else {
		 /* XXX Note, the below expression uses knowledge of
                    the layout of the string format, namely (nelem+1)
                    characters per string, followed by (nelem+1)
                    "undef" values. */
		  this->value.data.strptr[row][0] = 
		     theVar->value.data.strptr[0][elem+row];
		  this->value.data.strptr[row][1] = 0;  /* Null terminate */
	       }
	    } else {
	       fferror("Index out of range");
	       free( this->value.data.ptr );
	    }
	 }

      } else {

	 /* Reduce dimensions by 1, using a nonconstant expression */

	 for( row=0; row<gParse.nRows; row++ ) {

	    /* Index cannot be a constant */

	    if( theDims[0]->value.undef[row] ) {
	       fferror("Null encountered as vector index");
	       free( this->value.data.ptr );
	       break;
	    } else
	       dimVals[0] = theDims[0]->value.data.lngptr[row];

	    if( dimVals[0] < 1 ||
		dimVals[0] > theVar->value.naxes[ theVar->value.naxis-1 ] ) {
	       fferror("Index out of range");
	       free( this->value.data.ptr );
	    } else if ( this->type == BITSTR || this->type == STRING ) {
	      elem = this->value.nelem * (dimVals[0]-1);
	      elem += row*(theVar->value.nelem+1);
	      if (this->value.undef) 
		this->value.undef[row] = theVar->value.undef[row];
	      memcpy( (char*)this->value.data.strptr[0]
		      + row*sizeof(char)*(this->value.nelem+1),
		      (char*)theVar->value.data.strptr[0] + elem*sizeof(char),
		      this->value.nelem * sizeof(char) );
	      /* Null terminate */
	      this->value.data.strptr[row][this->value.nelem] = 0;
	    } else {
	       elem  = this->value.nelem * (dimVals[0]-1);
	       elem += row*theVar->value.nelem;
	       memcpy( this->value.undef + row*this->value.nelem,
		       theVar->value.undef + elem,
		       this->value.nelem * sizeof(char) );
	       memcpy( (char*)this->value.data.ptr
		       + row*dsize*this->value.nelem,
		       (char*)theVar->value.data.ptr + elem*dsize,
		       this->value.nelem * dsize );
	    }
	 }
      }
   }

   if( theVar->operation>0 ) {
     if (theVar->type == STRING || theVar->type == BITSTR) 
       free(theVar->value.data.strptr[0] );
     else 
       free( theVar->value.data.ptr );
   }
   for( i=0; i<nDims; i++ )
      if( theDims[i]->operation>0 ) {
	 free( theDims[i]->value.data.ptr );
      }
}

static void Do_GTI( Node *this )
{
   Node *theExpr, *theTimes;
   double *start, *stop, *times;
   long elem, nGTI, gti;
   int ordered;

   theTimes = gParse.Nodes + this->SubNodes[0];
   theExpr  = gParse.Nodes + this->SubNodes[1];

   nGTI    = theTimes->value.nelem;
   start   = theTimes->value.data.dblptr;
   stop    = theTimes->value.data.dblptr + nGTI;
   ordered = theTimes->type;

   if( theExpr->operation==CONST_OP ) {

      this->value.data.log = 
	(Search_GTI( theExpr->value.data.dbl, nGTI, start, stop, ordered, 0 )>=0);
      this->operation      = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      times = theExpr->value.data.dblptr;
      if( !gParse.status ) {

	 elem = gParse.nRows * this->value.nelem;
	 if( nGTI ) {
	    gti = -1;
	    while( elem-- ) {
	       if( (this->value.undef[elem] = theExpr->value.undef[elem]) )
		  continue;

            /*  Before searching entire GTI, check the GTI found last time  */
	       if( gti<0 || times[elem]<start[gti] || times[elem]>stop[gti] ) {
		 gti = Search_GTI( times[elem], nGTI, start, stop, ordered, 0 );
	       }
	       this->value.data.logptr[elem] = ( gti>=0 );
	    }
	 } else
	    while( elem-- ) {
	       this->value.data.logptr[elem] = 0;
	       this->value.undef[elem]       = 0;
	    }
      }
   }

   if( theExpr->operation>0 )
      free( theExpr->value.data.ptr );
}

static void Do_GTI_Over( Node *this )
{
   Node *theTimes, *theStart, *theStop;
   double *gtiStart, *gtiStop;
   double *evtStart, *evtStop;
   long elem, nGTI, gti, nextGTI;
   int ordered;

   theTimes = gParse.Nodes + this->SubNodes[0]; /* GTI times */
   theStop  = gParse.Nodes + this->SubNodes[2]; /* User start time */
   theStart = gParse.Nodes + this->SubNodes[1]; /* User stop time */

   nGTI     = theTimes->value.nelem;
   gtiStart = theTimes->value.data.dblptr;        /* GTI start */
   gtiStop  = theTimes->value.data.dblptr + nGTI; /* GTI stop */

   if( theStart->operation==CONST_OP && theStop->operation==CONST_OP) {

      this->value.data.dbl = 
	(GTI_Over( theStart->value.data.dbl, theStop->value.data.dbl,
		   nGTI, gtiStart, gtiStop, &gti));
      this->operation      = CONST_OP;

   } else {
      char undefStart = 0, undefStop = 0; /* Input values are undef? */
      double uStart, uStop;       /* User start/stop values */
      if (theStart->operation==CONST_OP) uStart = theStart->value.data.dbl;
      if (theStop ->operation==CONST_OP) uStop  = theStop ->value.data.dbl;

      Allocate_Ptrs( this );

      evtStart = theStart->value.data.dblptr;
      evtStop  = theStop ->value.data.dblptr;
      if( !gParse.status ) {

	 elem = gParse.nRows * this->value.nelem;
	 if( nGTI ) {
	    double toverlap = 0.0;
	    gti = -1;
	    while( elem-- ) {
	      if (theStart->operation!=CONST_OP) {
		undefStart = theStart->value.undef[elem];
		uStart     = evtStart[elem];
	      }
	      if (theStop->operation!=CONST_OP) {
		undefStop  = theStop ->value.undef[elem];
		uStop      = evtStop[elem];
	      }
	      /* This works because at least one of the values is not const */
	      if( (this->value.undef[elem] = (undefStart||undefStop)) )
		  continue;

            /*  Before searching entire GTI, check the GTI found last time  */
	       if( gti<0 || 
		   uStart<gtiStart[gti] || uStart>gtiStop[gti] ||
		   uStop <gtiStart[gti] || uStop >gtiStop[gti]) {
		 /* Nope, need to recalculate */
		 toverlap = GTI_Over(uStart, uStop, 
				     nGTI, gtiStart, gtiStop, 
				     &gti);
	       } else {
		 /* We are in same GTI, the overlap is just stop-start of user range */
		 toverlap = (uStop-uStart);
	       }

	       /* This works because at least one of the values is not const */
	       this->value.data.dblptr[elem] = toverlap;
	    }
	 } else
	    /* nGTI == 0; there is no overlap so set all values to 0.0 */
	    while( elem-- ) {
	       this->value.data.dblptr[elem] = 0.0;
	       this->value.undef[elem]       = 0;
	    }
      }
   }

   if( theStart->operation>0 ) {
     free( theStart->value.data.ptr );
   }
   if( theStop->operation>0 ) {
     free( theStop->value.data.ptr );
   }
}

static double GTI_Over(double evtStart, double evtStop,
		       long nGTI, double *start, double *stop,
		       long *gtiout)
{
  long gti1, gti2, nextGTI1, nextGTI2;
  long gti, nMax;
  double overlap = 0.0;

  *gtiout = -1L;
  /* Zero or negative bin size */
  if (evtStop <= evtStart) return 0.0;

  /* Locate adjacent GTIs for evtStart and evtStop */
  gti1 = Search_GTI(evtStart, nGTI, start, stop, 1, &nextGTI1);
  gti2 = Search_GTI(evtStop,  nGTI, start, stop, 1, &nextGTI2);

  /* evtStart is in gti1, we return that for future processing */
  if (gti1 >= 0) *gtiout = gti1;

  /* Both evtStart/evtStop are beyond the last GTI */
  if (nextGTI1 < 0 && nextGTI2 < 0) return 0.0;

  /* Both evtStart/evtStop are in the same gap between GTIs */
  if (gti1 < 0 && gti2 < 0 && nextGTI1 == nextGTI2) return 0.0;

  /* Both evtStart/evtStop are in the same GTI */
  if (gti1 >= 0 && gti1 == gti2) return (evtStop-evtStart);

  /* Count through the remaining GTIs; there will be at least one */
  /* The largest GTI to consider is either nextGTI2-1, if it exists,
     or nGTI-1 */
  if (nextGTI2 < 0) nMax = nGTI-1;
  else if (gti2 >= 0) nMax = nextGTI2;
  else nMax = nextGTI2-1;
  for (gti = nextGTI1; gti <= nMax; gti++) {
    double starti = start[gti], stopi = stop[gti];
    /* Trim the GTI by actual evtStart/Stop times */
    if (evtStart > starti) starti = evtStart;
    if (evtStop  < stopi ) stopi  = evtStop;
    overlap += (stopi - starti);
  }
    
  return overlap;
}

/*
 * Search_GTI - search GTI for requested evtTime
 * 
 * double evtTime - requested event time
 * long nGTI - number of entries in start[] and stop[]
 * double start[], stop[] - start and stop of each GTI
 * int ordered - set to 1 if time-ordered
 * long *nextGTI0 - upon return, *nextGTI0 is either
 *                   the GTI evtTime is inside
 *                   the next GTI if evtTime is not inside
 *                   -1L if there is no next GTI
 *                   not set if nextGTI0 is a null pointer
 *
 * NOTE: for *nextGTI to be well-defined, the GTI must
 *   be ordered.  This is true when called by Do_GTI.
 *
 * RETURNS: gti index that evtTime is located inside, or -1L
 */
static long Search_GTI( double evtTime, long nGTI, double *start,
			double *stop, int ordered, long *nextGTI0 )
{
   long gti, nextGTI = -1L, step;
                             
   if( ordered && nGTI>15 ) { /*  If time-ordered and lots of GTIs,   */
                              /*  use "FAST" Binary search algorithm  */
      if( evtTime>=start[0] && evtTime<=stop[nGTI-1] ) {
	 gti = step = (nGTI >> 1);
	 while(1) {
	    if( step>1L ) step >>= 1;
	    
	    if( evtTime>stop[gti] ) {
	       if( evtTime>=start[gti+1] )
		  gti += step;
	       else {
		  nextGTI = gti+1;
		  gti = -1L;
		  break;
	       }
	    } else if( evtTime<start[gti] ) {
	       if( evtTime<=stop[gti-1] )
		  gti -= step;
	       else {
		  nextGTI = gti;
		  gti = -1L;
		  break;
	       }
	    } else {
	       nextGTI = gti;
	       break;
	    }
	 }
      } else {
	 if (start[0] > evtTime) nextGTI = 0;
	 gti = -1L;
      }
      
   } else { /*  Use "SLOW" linear search.  Not required to be 
	        ordered, so we have to search the whole table
		no matter what.
	    */
      gti = nGTI;
      while( gti-- ) {
	if( stop[gti] >= evtTime ) nextGTI = gti;
	if( evtTime>=start[gti] && evtTime<=stop[gti] )
	    break;
      }
   }

   if (nextGTI >= nGTI) nextGTI = -1;
   if (nextGTI0) *nextGTI0 = nextGTI;

   return( gti );
}

static void Do_REG( Node *this )
{
   Node *theRegion, *theX, *theY;
   double Xval=0.0, Yval=0.0;
   char   Xnull=0, Ynull=0;
   int    Xvector, Yvector;
   long   nelem, elem, rows;

   theRegion = gParse.Nodes + this->SubNodes[0];
   theX      = gParse.Nodes + this->SubNodes[1];
   theY      = gParse.Nodes + this->SubNodes[2];

   Xvector = ( theX->operation!=CONST_OP );
   if( Xvector )
      Xvector = theX->value.nelem;
   else {
      Xval  = theX->value.data.dbl;
   }

   Yvector = ( theY->operation!=CONST_OP );
   if( Yvector )
      Yvector = theY->value.nelem;
   else {
      Yval  = theY->value.data.dbl;
   } 

   if( !Xvector && !Yvector ) {

      this->value.data.log =
	 ( fits_in_region( Xval, Yval, (SAORegion *)theRegion->value.data.ptr )
	   != 0 );
      this->operation      = CONST_OP;

   } else {

      Allocate_Ptrs( this );

      if( !gParse.status ) {

	 rows  = gParse.nRows;
	 nelem = this->value.nelem;
	 elem  = rows*nelem;

	 while( rows-- ) {
	    while( nelem-- ) {
	       elem--;

	       if( Xvector>1 ) {
		  Xval  = theX->value.data.dblptr[elem];
		  Xnull = theX->value.undef[elem];
	       } else if( Xvector ) {
		  Xval  = theX->value.data.dblptr[rows];
		  Xnull = theX->value.undef[rows];
	       }

	       if( Yvector>1 ) {
		  Yval  = theY->value.data.dblptr[elem];
		  Ynull = theY->value.undef[elem];
	       } else if( Yvector ) {
		  Yval  = theY->value.data.dblptr[rows];
		  Ynull = theY->value.undef[rows];
	       }

	       this->value.undef[elem] = ( Xnull || Ynull );
	       if( this->value.undef[elem] )
		  continue;

	       this->value.data.logptr[elem] = 
		  ( fits_in_region( Xval, Yval,
				    (SAORegion *)theRegion->value.data.ptr )
		    != 0 );
	    }
	    nelem = this->value.nelem;
	 }
      }
   }

   if( theX->operation>0 )
      free( theX->value.data.ptr );
   if( theY->operation>0 )
      free( theY->value.data.ptr );
}

static void Do_Vector( Node *this )
{
   Node *that;
   long row, elem, idx, jdx, offset=0;
   int node;

   Allocate_Ptrs( this );

   if( !gParse.status ) {

      for( node=0; node<this->nSubNodes; node++ ) {

	 that = gParse.Nodes + this->SubNodes[node];

	 if( that->operation == CONST_OP ) {

	    idx = gParse.nRows*this->value.nelem + offset;
	    while( (idx-=this->value.nelem)>=0 ) {
	       
	       this->value.undef[idx] = 0;

	       switch( this->type ) {
	       case BOOLEAN:
		  this->value.data.logptr[idx] = that->value.data.log;
		  break;
	       case LONG:
		  this->value.data.lngptr[idx] = that->value.data.lng;
		  break;
	       case DOUBLE:
		  this->value.data.dblptr[idx] = that->value.data.dbl;
		  break;
	       }
	    }
	    
	 } else {
	       
	    row  = gParse.nRows;
	    idx  = row * that->value.nelem;
	    while( row-- ) {
	       elem = that->value.nelem;
	       jdx = row*this->value.nelem + offset;
	       while( elem-- ) {
		  this->value.undef[jdx+elem] =
		     that->value.undef[--idx];

		  switch( this->type ) {
		  case BOOLEAN:
		     this->value.data.logptr[jdx+elem] =
			that->value.data.logptr[idx];
		     break;
		  case LONG:
		     this->value.data.lngptr[jdx+elem] =
			that->value.data.lngptr[idx];
		     break;
		  case DOUBLE:
		     this->value.data.dblptr[jdx+elem] =
			that->value.data.dblptr[idx];
		     break;
		  }
	       }
	    }
	 }
	 offset += that->value.nelem;
      }

   }

   for( node=0; node < this->nSubNodes; node++ )
     if( OPER(this->SubNodes[node])>0 )
       free( gParse.Nodes[this->SubNodes[node]].value.data.ptr );
}

/*****************************************************************************/
/*  Utility routines which perform the calculations on bits and SAO regions  */
/*****************************************************************************/

static char bitlgte(char *bits1, int oper, char *bits2)
{
 int val1, val2, nextbit;
 char result;
 int i, l1, l2, length, ldiff;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bits1);
 l2 = strlen(bits2);
 length = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(length+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bits1++);
    stream[i] = '\0';
    bits1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bits2++);
    stream[i] = '\0';
    bits2 = stream;
   }

 val1 = val2 = 0;
 nextbit = 1;

 while( length-- )
    {
     chr1 = bits1[length];
     chr2 = bits2[length];
     if ((chr1 != 'x')&&(chr1 != 'X')&&(chr2 != 'x')&&(chr2 != 'X'))
       {
        if (chr1 == '1') val1 += nextbit;
        if (chr2 == '1') val2 += nextbit;
        nextbit *= 2;
       }
    }
 result = 0;
 switch (oper)
       {
        case LT:
             if (val1 < val2) result = 1;
             break;
        case LTE:
             if (val1 <= val2) result = 1;
             break;
        case GT:
             if (val1 > val2) result = 1;
             break;
        case GTE:
             if (val1 >= val2) result = 1;
             break;
       }
 free(stream);
 return (result);
}

static void bitand(char *result,char *bitstrm1,char *bitstrm2)
{
 int i, l1, l2, ldiff, largestStream;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 largestStream = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(largestStream+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while ( (chr1 = *(bitstrm1++)) ) 
    {
       chr2 = *(bitstrm2++);
       if ((chr1 == 'x') || (chr2 == 'x'))
          *result = 'x';
       else if ((chr1 == '1') && (chr2 == '1'))
          *result = '1';
       else
          *result = '0';
       result++;
    }
 free(stream);
 *result = '\0';
}

static void bitor(char *result,char *bitstrm1,char *bitstrm2)
{
 int i, l1, l2, ldiff, largestStream;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 largestStream = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(largestStream+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while ( (chr1 = *(bitstrm1++)) ) 
    {
       chr2 = *(bitstrm2++);
       if ((chr1 == '1') || (chr2 == '1'))
          *result = '1';
       else if ((chr1 == '0') || (chr2 == '0'))
          *result = '0';
       else
          *result = 'x';
       result++;
    }
 free(stream);
 *result = '\0';
}

static void bitnot(char *result,char *bits)
{
   int length;
   char chr;

   length = strlen(bits);
   while( length-- ) {
      chr = *(bits++);
      *(result++) = ( chr=='1' ? '0' : ( chr=='0' ? '1' : chr ) );
   }
   *result = '\0';
}

static char bitcmp(char *bitstrm1, char *bitstrm2)
{
 int i, l1, l2, ldiff, largestStream;
 char *stream=0;
 char chr1, chr2;

 l1 = strlen(bitstrm1);
 l2 = strlen(bitstrm2);
 largestStream = (l1 > l2) ? l1 : l2;
 stream = (char *)malloc(sizeof(char)*(largestStream+1));
 if (l1 < l2)
   {
    ldiff = l2 - l1;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l1--    ) stream[i++] = *(bitstrm1++);
    stream[i] = '\0';
    bitstrm1 = stream;
   }
 else if (l2 < l1)
   {
    ldiff = l1 - l2;
    i=0;
    while( ldiff-- ) stream[i++] = '0';
    while( l2--    ) stream[i++] = *(bitstrm2++);
    stream[i] = '\0';
    bitstrm2 = stream;
   }
 while( (chr1 = *(bitstrm1++)) )
    {
       chr2 = *(bitstrm2++);
       if ( ((chr1 == '0') && (chr2 == '1'))
	    || ((chr1 == '1') && (chr2 == '0')) )
       {
          free(stream);
	  return( 0 );
       }
    }
 free(stream);
 return( 1 );
}

static char bnear(double x, double y, double tolerance)
{
 if (fabs(x - y) < tolerance)
   return ( 1 );
 else
   return ( 0 );
}

static char saobox(double xcen, double ycen, double xwid, double ywid,
		   double rot,  double xcol, double ycol)
{
 double x,y,xprime,yprime,xmin,xmax,ymin,ymax,theta;

 theta = (rot / 180.0) * myPI;
 xprime = xcol - xcen;
 yprime = ycol - ycen;
 x =  xprime * cos(theta) + yprime * sin(theta);
 y = -xprime * sin(theta) + yprime * cos(theta);
 xmin = - 0.5 * xwid; xmax = 0.5 * xwid;
 ymin = - 0.5 * ywid; ymax = 0.5 * ywid;
 if ((x >= xmin) && (x <= xmax) && (y >= ymin) && (y <= ymax))
   return ( 1 );
 else
   return ( 0 );
}

static char circle(double xcen, double ycen, double rad,
		   double xcol, double ycol)
{
 double r2,dx,dy,dlen;

 dx = xcol - xcen;
 dy = ycol - ycen;
 dx *= dx; dy *= dy;
 dlen = dx + dy;
 r2 = rad * rad;
 if (dlen <= r2)
   return ( 1 );
 else
   return ( 0 );
}

static char ellipse(double xcen, double ycen, double xrad, double yrad,
		    double rot, double xcol, double ycol)
{
 double x,y,xprime,yprime,dx,dy,dlen,theta;

 theta = (rot / 180.0) * myPI;
 xprime = xcol - xcen;
 yprime = ycol - ycen;
 x =  xprime * cos(theta) + yprime * sin(theta);
 y = -xprime * sin(theta) + yprime * cos(theta);
 dx = x / xrad; dy = y / yrad;
 dx *= dx; dy *= dy;
 dlen = dx + dy;
 if (dlen <= 1.0)
   return ( 1 );
 else
   return ( 0 );
}

/*
 * Extract substring
 */
int cstrmid(char *dest_str, int dest_len,
	    char *src_str,  int src_len,
	    int pos)
{
  /* char fill_char = ' '; */
  char fill_char = '\0';
  if (src_len == 0) { src_len = strlen(src_str); } /* .. if constant */

  /* Fill destination with blanks */
  if (pos < 0) { 
    fferror("STRMID(S,P,N) P must be 0 or greater");
    return -1;
  }
  if (pos > src_len || pos == 0) {
    /* pos==0: blank string requested */
    memset(dest_str, fill_char, dest_len);
  } else if (pos+dest_len > src_len) {
    /* Copy a subset */
    int nsub = src_len-pos+1;
    int npad = dest_len - nsub;
    memcpy(dest_str, src_str+pos-1, nsub);
    /* Fill remaining string with blanks */
    memset(dest_str+nsub, fill_char, npad);
  } else {
    /* Full string copy */
    memcpy(dest_str, src_str+pos-1, dest_len);
  }
  dest_str[dest_len] = '\0'; /* Null-terminate */

  return 0;
}


static void fferror(char *s)
{
    char msg[80];

    if( !gParse.status ) gParse.status = PARSE_SYNTAX_ERR;

    strncpy(msg, s, 80);
    msg[79] = '\0';
    ffpmsg(msg);
}

