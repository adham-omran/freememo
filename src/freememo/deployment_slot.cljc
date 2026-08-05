(ns freememo.deployment-slot
  "Client half of the optional deployment seam: a container a deployment may
   fill with its own content, and nothing at all by default.

   `freememo.deployment-hook` is the server half. Its head contribution is the
   ONLY thing that can define the activation function this namespace calls, so a
   build with no implementation renders no container and contacts nobody. That is
   the ORDINARY case, not a failure — every self-hosted build takes it, and so
   does dev, whose shells carry no placeholder.

   ## The gate is the function's existence, not a server flag

   A server-side flag could disagree with the markup: `index.dev.html` has no
   placeholder, so a dev checkout with the key set would render a container that
   nothing can ever fill. The head contribution that defines the function is the
   same thing a flag would be derived from, so reading the function directly
   makes the two impossible to desynchronize.

   `activation-global` is FROZEN WIRE FORMAT. The two halves deploy
   independently, so renaming it breaks every page served by the older half —
   the same reasoning that keeps the implementation's session key named after a
   namespace that no longer exists.

   ## Why plain `defn`s, split at the top level

   Electric re-evaluates reactive bodies unpredictably, so the CALLER owns
   once-per-mount semantics with `e/snapshot`; an `e/defn` here would repeat the
   side effect. A `#?(:cljs …)` inside an `e/defn` body would also diverge the
   CLJ and CLJS frame slot counts, which is why the platform split lives at the
   top level — the same shape as `freememo.modal-shell`'s focus helpers.

   ## Placing a container

   Render an element carrying the `deployment-slot` class, then call
   `activate-slot!` once on its node with one of the slot names below. The class
   owns the box and sets no height, so an unfilled container measures zero and
   leaves no trace. Nothing may be rendered INSIDE the container: the activation
   function treats an existing element child as \"already activated\" and
   declines.

   Electric must own the container. A container appended by plain JavaScript into
   a parent whose children churn makes Electric remove the wrong node and strand
   orphaned subtrees, silently. Injecting into an Electric-created LEAF is safe,
   which is exactly what this seam does. See
   plans/deployment-slot-home-band.md sections 3.2 and 3.3."
  #?(:cljs (:require [goog.object :as gobj])))

(def activation-global
  "Name of the function a deployment's head contribution may define on `window`.

   FROZEN: public code and the implementation deploy independently, so a rename
   breaks whichever half deploys second. Read by string key, never as a property
   access, so advanced compilation cannot rename it (same reason
   `freememo.vendor-libs` probes globals by string)."
  "__freememoDeploymentSlot")

(def home-bottom
  "The empty band under the Home page's quick links.

   Slot names are part of the same frozen contract as `activation-global`: the
   implementation maps this exact string to what it puts there, and an unknown
   name activates nothing."
  "home-bottom")

#?(:cljs
   (defn- activation-fn
     "The activation function this deployment defined, or nil.
      Post: nil, or a JavaScript function of [node slot-name]."
     []
     (let [f (gobj/get js/window activation-global)]
       (when (fn? f) f))))

#?(:cljs
   (defn slot-available?
     "Whether this deployment supplies slots at all.
      Post: true only when calling `activate-slot!` can do something. False in
      dev and in every build without the implementation, so the caller renders no
      container."
     []
     (some? (activation-fn))))
#?(:clj (defn slot-available? [] false))

#?(:cljs
   (defn activate-slot!
     "Let this deployment fill `node` with the content it assigns to `slot-name`.

      Pre: `node` is an element Electric created and left empty, and `slot-name`
      is one of the names this namespace defines. Call once per mount, through
      `e/snapshot`.
      Post: nil. Does nothing when no implementation is present, when the
      implementation assigns nothing to `slot-name`, or when `node` already holds
      an element child. Idempotent per node, so a repeated call cannot stack two
      units."
     [node slot-name]
     (when-some [f (activation-fn)]
       (when node (f node slot-name)))
     nil))
#?(:clj (defn activate-slot! [_node _slot-name] nil))
