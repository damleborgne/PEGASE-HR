#!/bin/sh
# moduledep.sh -- script that computes dependencies on Fortran 90 modules

# files whose dependencies must be computed
sources=`ls *.f90`

# files that may contain modules
# extra directories can be specified on the command line
sources_all="$sources"
for dir in $*
do
  sources_all="$sources_all `ls $dir/*.f90`"
  echo $sources_all
done

rm -f moduledep.tmp1 moduledep.tmp2 # destroy previous contents

# create list of module dependencies
# each line is of the form:
# file_name.o : @module_name@
# cast all module names to lowercase because Fortran is case insensitive
egrep -i "^ *use " $sources |             # look for "USE name"
sed 's/f90:/o /
     s/,/ /' |                            # replace extension, insert space
#                                         #   and remove trailing comma
awk '{print $1 " : @" tolower($3) "@"}' | # create dependency entry
sort | uniq > moduledep.tmp1              # remove duplicates

# create list of available modules
# for each module, create a line of the form:
# s/@module_name@/file_name/g
egrep -i "^ *module " $sources_all |           # look for "MODULE name"
sed 's/f90:/o /
     s/\//\\\//g' |                            # replace extension, insert
#                                              #   space and escape slashes
awk '{print "s/@" tolower($3) "@/" $1 "/" }' | # create substitution line
sort | uniq > moduledep.tmp2                   # remove duplicates

# replace module names with file names
# by applying the file of substitution patterns just created
sed -f moduledep.tmp2 moduledep.tmp1 |
awk '{if ($1 != $3) print}' |          # remove self dependencies
sort | uniq                            # remove duplicates

rm -f moduledep.tmp1 moduledep.tmp2 # remove temporary files
