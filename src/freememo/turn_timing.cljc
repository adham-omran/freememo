(ns freememo.turn-timing
  "The client's wall clock, and how long one Quiz Review turn took.

   `kg_reviews.duration_ms` is the column; this namespace is the only producer of
   what goes in it. Two plain fns, no requires — both `freememo.quiz-page` and
   `freememo.quiz-card-turn` need them and neither may require the other.

   `start-ms` has a second reader with no duration in it: the exam countdown's
   deadline. One reading of the clock, one name — a private copy in quiz-page
   said the same thing in the same words and was free to drift from this one.

   The measured interval is turn mount → submit-or-rating, which is what Anki
   records for a card (question shown → answered). On the self-rated arm that
   deliberately spans the reveal: revealing is part of answering.

   Call `start-ms` through `e/snapshot` at the top of a turn's `let`, never as a
   body expression — Electric re-runs body expressions unpredictably, and a restart
   mid-turn would silently under-report (CLAUDE.md, JS Library Init Side Effects).

   The value crosses to an untrusted-peer boundary, so nothing here bounds it.
   `db/clamp-duration-ms` is the single bound, applied at the one write site.")

(defn start-ms
  "Epoch milliseconds now — a turn's start mark, or the exam clock's reading.
   Pre:  called from client-sited code. Every caller is inside an `e/client` body;
         the :clj arm exists so the var resolves on both peers, not to be read.
   Post: a positive epoch ms on the client; 0 on the server, which elapsed-ms
         reads as \"not timed\" and a countdown would read as 1970."
  []
  #?(:cljs (js/Date.now) :clj 0))

(defn elapsed-ms
  "Milliseconds since `start`.
   Pre:  `start` came from start-ms in the same turn frame.
   Post: a non-negative long, or nil when the turn was not timed — a 0 `start`
         (the server peer's value) or a clock that ran backwards both mean
         \"not recorded\", which is what NULL in the column means."
  [start]
  #?(:cljs (when (and (number? start) (pos? start))
             (let [d (- (js/Date.now) start)]
               (when (>= d 0) d)))
     :clj nil))
