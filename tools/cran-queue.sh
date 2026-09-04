#!/bin/sh
# Where is concurve sitting in CRAN's incoming queue?
#
# CRAN's incoming directory is publicly browsable, so submission status can
# be checked without waiting on email. Run this from anywhere:
#
#     sh tools/cran-queue.sh
#
# What each folder means for a submission:
#
#   pretest/   automated incoming checks running
#   newbies/   awaiting manual review -- where returning archived
#              packages and first-time submitters land
#   inspect/   a reviewer has opened it
#   pending/   waiting on the maintainer to do something
#   recheck/   being re-checked after a change
#   waiting/   blocked on something else (often a reverse dependency)
#   publish/   accepted, about to appear on CRAN
#   archive/   rejected, withdrawn, or superseded by a later upload
#
# Movement here is driven by CRAN volunteers acting by hand, not by a
# queue runner, so nothing changing over a few hours means nothing.

set -u

PKG=concurve
DIRS="pretest newbies inspect pending recheck waiting publish archive special"
BASE=https://cran.r-project.org/incoming

printf '%s incoming queue, %s\n\n' "$PKG" "$(date -u '+%Y-%m-%d %H:%M UTC')"

found=""
for d in $DIRS; do
  hit=$(curl -sS --max-time 25 "$BASE/$d/" 2>/dev/null \
        | grep -io "${PKG}_[0-9][0-9.]*\.tar\.gz" | sort -u | tr '\n' ' ')
  printf '  %-10s %s\n' "$d/" "${hit:-—}"
  [ -n "$hit" ] && found="$found$d "
done

printf '\nOn CRAN now? '
if curl -sS --max-time 25 "https://cran.r-project.org/web/packages/$PKG/index.html" 2>/dev/null \
     | grep -q "was removed from the CRAN repository"; then
  echo "no - still shows as archived"
else
  echo "YES - the package page no longer reports removal"
fi

printf '\nsitting in: %s\n' "${found:-nothing found}"
