var Elm = require('./elm/Main.elm').Elm;

Elm.Main.init({
  node: document.getElementById("parlezvous-comments"),
  flags: {
    origin: document.location.host,
  }
});

