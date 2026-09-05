#!/bin/sh
# Where is concurve sitting in CRAN's incoming queue, and what should be
# done about it?
#
#     sh tools/cran-queue.sh
#
# CRAN's incoming directory is publicly browsable, so submission status can
# be checked without waiting on email. Movement is driven by CRAN
# volunteers acting by hand, not by a queue runner, so nothing changing
# over a few hours means nothing.
#
# The recommended action printed at the end mirrors the decision table in
# AGENTS.md ("State"). It assumes the situation as of 2026-09-04: 3.0.3 is
# in the queue with a known curve_lik_glm() dispersion bug, a withdrawal
# request has been drafted at dev/cran-withdraw-3.0.3.md, and
# release/3.0.4 holds the fix. Revisit this script once that resolves.

set -u

PKG=concurve
PENDING_VERSION=3.0.3
DIRS="pretest newbies inspect pending recheck waiting publish archive special"
BASE=https://cran.r-project.org/incoming

printf '%s incoming queue, %s\n\n' "$PKG" "$(date -u '+%Y-%m-%d %H:%M UTC')"

where=""
for d in $DIRS; do
  hit=$(curl -sS --max-time 25 "$BASE/$d/" 2>/dev/null \
        | grep -io "${PKG}_[0-9][0-9.]*\.tar\.gz" | sort -u | tr '\n' ' ')
  printf '  %-10s %s\n' "$d/" "${hit:-—}"
  case "$hit" in
    *"${PKG}_${PENDING_VERSION}.tar.gz"*) where="$d" ;;
  esac
done

printf '\nOn CRAN now? '
if curl -sS --max-time 25 "https://cran.r-project.org/web/packages/$PKG/index.html" 2>/dev/null \
     | grep -q "was removed from the CRAN repository"; then
  on_cran=no
  echo "no - the package page still reports removal"
else
  on_cran=yes
  echo "YES - the package page no longer reports removal"
fi

echo
echo "-------------------------------------------------------------------"
printf '%s %s is ' "$PKG" "$PENDING_VERSION"
case "$where" in
  pretest|newbies)
    echo "in ${where}/ -- still queued."
    echo
    echo "ACTION: nothing to do. It is awaiting manual review; returning"
    echo "  archived packages land in newbies/. If the withdrawal request"
    echo "  has not been sent yet, send it:"
    echo "      open dev/cran-withdraw-3.0.3.eml"
    echo "  Do not submit 3.0.4 while this sits here."
    ;;
  inspect|pending|recheck|waiting)
    echo "in ${where}/ -- a reviewer has it open."
    echo
    echo "ACTION: expect an email from a CRAN volunteer, and reply the same"
    echo "  day. concurve was archived in 2022 for slow responses, so"
    echo "  turnaround is itself being assessed. Still do not submit 3.0.4."
    ;;
  publish)
    echo "in publish/ -- accepted, about to appear on CRAN."
    echo
    echo "ACTION: the withdrawal did not land in time. 3.0.4 becomes a fast"
    echo "  bug-fix follow-up; the curve_lik_glm() dispersion bug (intervals"
    echo "  up to 5x too narrow, depending on the units of the response)"
    echo "  justifies the short interval. Merge release/3.0.4 and submit."
    ;;
  archive)
    echo "in archive/ -- withdrawn, rejected, or superseded."
    echo
    echo "ACTION: the way is clear for 3.0.4. Merge release/3.0.4 (draft"
    echo "  PR #60), confirm CI is green, then:"
    echo "      devtools::submit_cran()"
    echo "  cran-comments.md is already written for 3.0.4."
    ;;
  "")
    if [ "$on_cran" = yes ]; then
      echo "not in the queue, and the package page is live -- it published."
      echo
      echo "ACTION: treat 3.0.4 as a follow-up release; see the publish/ case."
    else
      echo "not in the queue, but the page still shows as archived."
      echo
      echo "ACTION: ambiguous. Check email for a CRAN decision before doing"
      echo "  anything -- it may have been rejected without reaching archive/."
    fi
    ;;
esac
echo "-------------------------------------------------------------------"
