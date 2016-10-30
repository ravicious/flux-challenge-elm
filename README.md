# flux-challenge-elm
Flux Challenge done in Elm

## Caveats

[flux-challenge readme](https://github.com/staltz/flux-challenge/blob/master/README.md) says the app
is supposed to cancel HTTP requests for jedis in some cases. In Elm, aborting a request is not
straightforward to accomplish. To work around that, flux-challenge-elm behavior differs from what's
specified in flux-challenge readme.

> Do not make any request for a sith that would be outside the list.
> Cancel any such obsolete request when the user scrolls.

Instead of canceling the requests, we don't add a jedi to the roster if it's not a master of
the first jedi or the apprentice of the last jedi.

> When either the current planet indicator changes OR loaded new rows: check if there is a displayed
> Dark Jedi whose home planet matches the current planet. If true, then display that Dark Jedi in
> red text, and cancel ALL ongoing HTTP requests for rows. Freeze the UI from scrolling until the
> current planet changes again and there is no red-highlighted Dark Jedi anymore.

Instead of canceling the ongoing requests, we don't add any new jedis to the roster when the UI
is frozen.
