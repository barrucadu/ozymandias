# Errors

API errors are returned in JSON as a [problem details object][], in
the following format:

```json
{
  "type": "URI which identifies the problem type and points to further information",
  "title": "Short human-readable summary of the problem type",
  "detail": "Human-readable explanation of this specific instance of the problem."
}
```

There may be additional fields named `detail_KEY`.

[problem details object]: https://tools.ietf.org/html/rfc7807

## Podman request returned invalid JSON

Podman returned a successful response but with an unexpected response
body.

**This could be caused by:**

- an API incompatibility
- a bug in Ozymandias
- a bug in Podman

**Before reporting an issue:**

Check you have the latest version of Ozymandias and Podman, and that
Podman is working.

## Podman request raised an HTTP error

Podman returned an unexpected response.

**This could be caused by:**

- an API incompatibility
- a bug in Ozymandias
- a bug in Podman

**Before reporting an issue:**

Check you have the latest version of Ozymandias and Podman, and that
Podman is working.

## Invalid URL

Ozymandias was unable to query a necessary API because it constructed
an invalid URL.

**This could be caused by:**

- incorrect configuration
- a bug in Oxymandias

**Before reporting an issue:**

Check that any URLs you are passing to Ozymandias are correct.