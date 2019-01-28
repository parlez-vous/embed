var Elm = require('./elm/Main.elm').Elm;

require('./parlez.css')

// .embed() can take an optional second argument.
// This would be an object describing the data we need to start a program
// i.e. a userID or some token
Elm.Main.init({
  node: document.getElementById("parlez-embed")
});
