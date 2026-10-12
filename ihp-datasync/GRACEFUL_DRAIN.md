# Cooperative DataSync message draining

`IHP.DataSync.Drain.newDrainSignal` creates a one-way process-local latch.
Pass it as the first argument to
`IHP.DataSync.ControllerImpl.runDataSyncControllerWithDrain`; all remaining
arguments are unchanged. Share the signal across the connections to stop.
`requestDrain signal` closes admission, but does not itself wait for completion.
Await each controller action before closing its socket or stopping its server.
The existing `runDataSyncController` API looks up an optional `DrainSignal` in
framework configuration. Initialize it once with `liftIO newDrainSignal`, then
`option signal`; default, native and custom controllers using that entrypoint
automatically share the configured latch without replacing controller instances.
The administrative endpoint calls `requestDrain signal` on that same value.
Without the option the existing entrypoint creates a private open latch.

The receive callback must be cancellation-safe and must not perform business
effects. Drain interrupts only that callback, then joins accepted message
handlers, including handlers waiting for the concurrency semaphore. Acceptance
is linearized against the latch; a message received concurrently with closure
is rechecked before spawning. A closed signal also rejects new connections'
message loops. Handler handles are registered under masking and removed only
after completion, avoiding child deregistration racing parent registration.

Ordinary receive failure and parent cancellation still cancel and join handlers.
They are not graceful completion. Uncaught handler failures propagate. Handlers
that catch errors themselves retain their existing protocol behavior: a normal
return proves joining, not successful or unambiguous external effects.

Read-only subscription handlers finish setup without cancellation, then their
idle wait ends on either the client close signal or the shared drain signal.
Their normal unsubscribe finalizer runs. A scoped callback gate also joins
already dispatched notification callbacks and makes late invocations no-ops.
This covers subscriptions accepted but still waiting for a semaphore slot.

## Explicit limitations

This is a lifecycle primitive, not a complete deployment drain:

- Configure the shared latch before admitting connections. Controllers calling
  the explicit `WithDrain` entrypoint must pass that same latch.
- Open-transaction handlers are joined too, and can block forever once message
  admission closes. Safe transaction rollback requires a separate protocol
  integration after accepted mutations and commits have joined; do not
  replace waiting with cancellation and claim success.
- Other detached work and server lifecycle cleanup are not covered.
- HTTP admission, Warp shutdown policy, durable process-generation completion
  evidence, and recovery of uncertain effects remain application responsibilities.

Focused tests are in `Test/DataSync/DrainSpec.hs`; they need no database or server.
