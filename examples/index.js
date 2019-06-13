import { Elm } from "./src/Main.elm";
import { registerCustomElement, registerPorts } from "elm-mapbox";

import "mapbox-gl/dist/mapbox-gl.css";

const token =
    "pk.eyJ1IjoiZm9yZXN0LXByb3RlY3Rvci1nYW1lIiwiYSI6ImNqdnMxMmE4aTBtM3A0YW1zc2gxaDZwMTkifQ.5GqU-G9npYW3UeCxQYrV8g";

registerCustomElement({
    token
});

var app = Elm.Main.init({ flags: {} });
// registerPorts(app);
