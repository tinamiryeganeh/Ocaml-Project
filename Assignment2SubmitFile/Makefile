# $Id: Makefile,v 1.5 2021-10-16 14:27:17-07 - - $

#
# Generally useful macros
#

MKFILE     = Makefile
MAKEFLAGS += --no-builtin-rules
DEPSFILE   = ${MKFILE}.deps
NOINCLUDE  = ci clean spotless check
NEEDINCL   = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
GMAKE      = ${MAKE} --no-print-directory
OCAMLOPT   = ocamlopt -g

#
# File macros
#

EXECBIN    = mbinterp
OBJCMX     = etc.cmx parser.cmx scanner.cmx tables.cmx \
             debug.cmx interp.cmx main.cmx
OBJCMI     = ${OBJCMX:.cmx=.cmi} absyn.cmi
OBJBIN     = ${OBJCMX:.cmx=.o}
MLSOURCE   = absyn.mli etc.mli etc.ml tables.mli tables.ml \
             debug.mli debug.ml interp.mli interp.ml main.ml
GENLEXYACC = parser.mli parser.ml scanner.ml
GENSOURCE  = debug.mli tables.mli ${GENLEXYACC}
GENFILES   = ${GENSOURCE} parser.output ${DEPSFILE}
OTHERFILES = ${MKFILE} using .ocamlinit
ALLSOURCES = ${MLSOURCE} parser.mly scanner.mll ${OTHERFILES}
LISTING    = Listing.ps

SCRIPTBIN  = /afs/cats.ucsc.edu/courses/cse112-wm/bin
OCAMLBIN   = /afs/cats.ucsc.edu/courses/cse112-wm/usr/ocaml-4.09.0/bin/
export PATH := ${PATH}:${SCRIPTBIN}:${OCAMLBIN}

#
# General targets
#

all : ${EXECBIN}

${EXECBIN} : ${OBJCMX} ${OBJBIN}
	${OCAMLOPT} str.cmxa ${OBJCMX} -o ${EXECBIN}

%.cmi : %.mli
	${OCAMLOPT} -c $<

%.o %.cmx : %.ml
	${OCAMLOPT} -c $<

%.ml : %.mll
	ocamllex $<

%.mli %.ml : %.mly
	ocamlyacc -v $<


MAKEMLI    = ( echo "(* $@ created by: ${OCAMLOPT} -i $< *)" \
             ; echo "(* date: $$(LC_TIME=C date) *)" \
	     ; ${OCAMLOPT} -i $< \
             ) >$@

tables.mli : tables.ml absyn.cmi etc.cmi
	${call MAKEMLI}

debug.mli : debug.ml absyn.cmi
	${call MAKEMLI}

#
# Misc targets
#

path :
	echo ${PATH}

clean :
	- rm ${OBJCMI} ${OBJCMX} ${OBJBIN} ${GENSOURCE}

spotless : clean
	- rm ${EXECBIN} ${GENFILES} ${LISTING} ${LISTING:.ps=.pdf} 

check : ${ALLSOURCES}
	checksource ${ALLSOURCES}

ci : ${ALLSOURCES}
	cid -is ${ALLSOURCES}

deps : ${MLSOURCE} ${GENSOURCE}
	@ echo "# ${DEPSFILE} created: $$(date)" >${DEPSFILE}
	ocamldep ${MLSOURCE} ${GENSOURCE} >>${DEPSFILE}

${DEPSFILE} : tables.mli
	@touch ${DEPSFILE}
	${GMAKE} deps

lis : ${ALLSOURCES} ${DEPSFILE}
	mkpspdf ${LISTING} ${ALLSOURCES}

again :
	${GMAKE} spotless
	${GMAKE} deps
	${GMAKE} ci
	${GMAKE} all
	${GMAKE} lis

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif

