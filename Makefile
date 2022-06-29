#
#          Make this library and documentation independently of IntelliJ
#

MAIN   := RegexKit
JAR    := out/$(MAIN).jar
PKG    := sufrin.regexkit

SOURCEPATH   := src
SOURCEFILES  := $(wildcard */*/*/*.scala */*/*/*/*.scala */*/*/*/*/*.scala)

#
# location of the IntelliJ-compiled class files
#
IJOUT    := ./out/production/Regexkit
#
#
#

#
#
include Makefile-Scala
#
#  

