(ns boh.repository-proxy)

(defprotocol RepositoryProxy

  "This is the contract by which we abstract 'speaking' to an (often
  remote) repository.

  `pull` is expected to return a single use `core.async` channel
  containing the diff since the version requested.

  `push` is expected to return a single use `core.async` channel
  containing the result of pushing a diff the repository. What this
  means is oft up to the 'merge strategy' employed on the repository
  itself. At the very least, this result should contain the
  post-operation version (heads) of the repository.

  `subscribe` is similar to `pull` with the exception that the
  returned `core.async` persists past the first 'request' and can be
  listened to for additional diffs as they are broadcast from the
  proxied repository."

  (pull [proxy version])
  (push [proxy diff])
  (subscribe [proxy version]))
