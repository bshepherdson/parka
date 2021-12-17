(ns parka.streams
  "Helper stream library for parsing, they're maps, like this:

  | key | description |
  | --- | ----------- |
  | `:str` | Entire input string |
  | `:pos` | Current index into that string |
  | `:filename` | Source file name |
  | `:line` | Current line number (1-based) |
  | `:col` | Current column number (0-based) |
  | `:value` | Arbitrary parsing result (optional) |
  | `:state` | Arbitrary user state (optional) |"
  (:require
    [parka.errors :as errs]))

(defn at-eof?
  "Returns whether the stream is at EOF."
  [{:keys [pos str]}]
  (>= pos (count str)))

(defn head
  "Given a stream, returns `[head-char EOF?]`. `head-char` is `nil` at EOF."
  [{:keys [str pos]}]
  (if (>= pos (count str))
    [nil true]
    [(nth str pos)]))

(defn tail
  "Returns a new stream, 1 character farther along than the input."
  ; TODO Memoize this?
  [{:keys [str pos col line] :as s}]
  (if (= \newline (nth str pos))
    (assoc s
           :pos  (inc pos)
           :line (inc line)
           :col  0)
    (assoc s
           :pos (inc pos)
           :col (inc col))))

(defn set-value
  "Returns a new stream with the given `:value` attached."
  [s value]
  (assoc s :value value))

(defn stream
  "Creates a new stream for the given `filename`, aimed at the start of the
  string `input`. An optional user state can be provided."
  ([filename input] (stream filename input {}))
  ([filename input state]
   {:str input
    :pos 0
    :filename filename
    :line 1
    :col 0
    :state state}))

;(defn char-seq
;  "Returns a lazy seq of the head characters of successive tail streams."
;  [s]
;  (lazy-seq
;    (let [[h eof?] (head s)]
;      (if eof?
;        nil
;        (cons h (char-seq (tail s)))))))
;
;(defn drop-chars
;  "Returns the stream created by dropping n characters from s."
;  [s n]
;  (if (pos? n)
;    (recur (tail s) (dec n))
;    s))

(defn expect-char
  "Matches a predicate `pred` against a stream. Returns the character if it
  matches.
  Throws at EOF or if the next character doesn't match the predicate.
  The error message names the missing expectation as `desc`."
  [s pred desc]
  (let [[h eof?] (head s)]
    (if (or eof? (not (pred h)))
      (errs/failed-expect s [desc])
      (tail s))))

(defn remaining-input
  "Returns the un-parsed tail of the input as a single string."
  [{:keys [str pos]}]
  (.substring str pos))

