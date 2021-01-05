var Elm = require('./elm/Main.elm').Elm;

var apiEndpoint = process.env.API_ENDPOINT

// for error-reporting purposes
var gitRef = process.env.GIT_REF || null


if (!gitRef && process.env.NODE_ENV === 'production') {
  throw new Error('Missing git ref for production environment')
}

const srcUrl = new URL(document.location.href)
const hostUrl = new URL(srcUrl.searchParams.get('host_url'))

const notifyParentFrame = (data) => {
  window.parent.postMessage(data, hostUrl.origin)
}

let currentHeight = window.innerHeight
notifyParentFrame({ height: currentHeight })


const observerOptions = {
  subtree: true,
  childList: true,
  attributes: true,
}

const domChangeObserver = new MutationObserver(() => {
  const newHeight = document.body.offsetHeight
  if (newHeight !== currentHeight) {
    notifyParentFrame({ height: newHeight })

    currentHeight = newHeight 
  }
})

domChangeObserver.observe(document.body, observerOptions)

var sessionTokenKey = 'sessionToken'

var app = Elm.Main.init({
  node: document.getElementById("parlezvous-comments"),
  flags: {
    apiEndpoint,
    siteUrl: hostUrl.origin,
    anonymousUsername: localStorage.getItem('anonymousUsername'),
    gitRef,
    sessionToken: localStorage.getItem(sessionTokenKey),
  }
});

app.ports.writeToLocalStorage.subscribe(([key, value]) => {
  localStorage.setItem(key, value)
})

app.ports.removeToken.subscribe(() => {
  localStorage.removeItem(sessionTokenKey)
})

