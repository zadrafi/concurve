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
# The version under review is DISCOVERED from the queue rather than pinned
# in this file, so the script does not go stale when a submission
# resolves. Anything in archive/ is historical -- rejected, withdrawn, or
# superseded -- so the live submission is whatever sits in one of the
# other folders.
#
# The one piece of version-specific advice is the withdrawal note for
# 3.0.3, which carries a known curve_lik_glm() dispersion bug. It is gated
# on 3.0.3 actually being the version in the queue, so it disappears by
# itself once that resolves. See AGENTS.md, "State".

set -u

PKG=concurve
BASE=https://cran.r-project.org/incoming
QUEUE_DIRS="pretest newbies inspect pending recheck waiting publish"
ALL_DIRS="$QUEUE_DIRS archive special"

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." 2>/dev/null && pwd) || ROOT=.
src_ver=$(awk '/^Version:/ {print $2; exit}' "$ROOT/DESCRIPTION" 2>/dev/null)

printf '%s incoming queue, %s\n' "$PKG" "$(date -u '+%Y-%m-%d %H:%M UTC')"
[ -n "$src_ver" ] && printf 'local source is at %s\n' "$src_ver"
echo

live_dir=""
live_ver=""
for d in $ALL_DIRS; do
  hit=$(curl -sS --max-time 25 "$BASE/$d/" 2>/dev/null \
        | grep -io "${PKG}_[0-9][0-9.]*\.tar\.gz" | sort -u | tr '\n' ' ')
  printf '  %-10s %s\n' "$d/" "${hit:-—}"

  # A submission anywhere but archive/ is the one still under review.
  case " $QUEUE_DIRS " in
    *" $d "*)
      if [ -n "$hit" ] && [ -z "$live_ver" ]; then
        live_dir=$d
        # Highest version wins, compared numerically field by field. A
        # plain lexical sort gets this wrong in the ordinary case: it
        # would rank 3.0.3 before 3.0.4 and pick the older one. sort -V
        # would do the job but is not portable (absent on some BSD
        # sorts), so compare the dot-separated fields as numbers. The
        # fourth field covers development versions like 3.0.0.9000.
        live_ver=$(printf '%s\n' $hit \
          | sed "s/^${PKG}_//; s/\.tar\.gz$//" \
          | sort -t. -k1,1n -k2,2n -k3,3n -k4,4n \
          | tail -1)
      fi
      ;;
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

if [ -z "$live_ver" ]; then
  if [ "$on_cran" = yes ]; then
    echo "Nothing of $PKG is in the queue, and the package page is live."
    echo
    echo "ACTION: the last submission published. Any fix in hand becomes a"
    echo "  normal follow-up release -- bump the version, update"
    echo "  cran-comments.md, and submit when ready."
  else
    echo "Nothing of $PKG is in the queue, but the page still shows removed."
    echo
    echo "ACTION: ambiguous. Check email for a CRAN decision before doing"
    echo "  anything; a rejection does not always leave a trace in archive/."
  fi
else
  printf '%s %s is in %s/' "$PKG" "$live_ver" "$live_dir"
  [ -n "$src_ver" ] && [ "$src_ver" != "$live_ver" ] \
    && printf ' (local source is ahead, at %s)' "$src_ver"
  echo "."
  echo

  case "$live_dir" in
    pretest|newbies)
      echo "ACTION: nothing to do -- it is awaiting review. Returning"
      echo "  archived packages land in newbies/. Do not submit a newer"
      echo "  version while this sits here: a version number cannot be"
      echo "  reused with different contents, and CRAN asks maintainers not"
      echo "  to resubmit while a submission is pending."
      ;;
    inspect|pending|recheck|waiting)
      echo "ACTION: a reviewer has it open. Expect email and reply the same"
      echo "  day -- concurve was archived in 2022 for slow responses, so"
      echo "  turnaround is itself being assessed. Do not submit meanwhile."
      ;;
    publish)
      echo "ACTION: accepted, about to appear on CRAN. Any fix in hand"
      echo "  becomes a follow-up release. A correctness bug justifies a"
      echo "  shorter interval than CRAN's usual 1-2 months between"
      echo "  releases; say so in cran-comments.md."
      ;;
  esac

  # Version-specific: 3.0.3 has a known dispersion bug in curve_lik_glm().
  if [ "$live_ver" = "3.0.3" ]; then
    echo
    echo "  NOTE, specific to 3.0.3: it carries a curve_lik_glm() dispersion"
    echo "  bug making support intervals depend on the units of the response"
    echo "  (up to 5x too narrow). release/3.0.4 has the fix. If the"
    echo "  withdrawal request has not been sent yet:"
    echo "      open dev/cran-withdraw-3.0.3.eml"
    echo "  It must come from the maintainer's registered address."
  fi
fi
echo "-------------------------------------------------------------------"
