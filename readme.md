# gampleman/elm-geospatial

Welcome! elm-geospatial is a library for geospatial analysis. That means that it provides:

- **types** to represent geographical information inside your app
- **decoders** and **encoders** to get your data in and out of standard geospatial formats (currently that means *geojson*)
- **functions** that allow you to measure, transform, convert, generate, join, classify, aggregate and interpolate that data

## Concepts

At the core of the data model is a `Feature coordinates data`. You can think of it as something that associates some actual geography with some arbitrary data about that geography. These typically come in collections, we call ours a `GeoCollection`.
It's really just a type alias to `List (Feature coordinates data)`, but there are some convenient features that
make more sense at the collection level (like the decoders/encoders).
There are three types of geography that a Feature can contain: `Points`, `LineStrings` and `Polygons`. These are all
expressed in coordinates. The standard type of coordinate in the library is the familiar `Latitude` / `Longitude` pairs, also known as `WSG84`. Since apparently no one can ever agree if latitude or longitude comes first, we use a record for these. We also avoid raw floats in most places and use units from [ianmackenzie/elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/).

However, coordinates are expressed as a type variable, so you can use this code even if you need to use some other coordinate reference system.

## FAQ

(Hey! This module isn't even released. Nobody asked me anything about this. But it's kinda convenient to write thos way, so bear with me.)

**Why not just use mgold/elm-geojson instead?**

Depends on what you need. But that library doesn't do anything with GeoJSON properties, which in practical geographical dashboards tend to be super important. So it can serve as a help in building your own data structures as an intermediate decoder. This library on the other hand is designed to provide a representation you can actually use in your application code. Think of it as a `List`, but for geo data.

## Roadmap

You may have noticed that the library is relatively sparse at the moment. That's because its in a really early stage.
Here's what should happen:

- [ ] Refine the types to strike a good balance between flexibility, safety and convenience. Currently the idea is that the types will not be opaque so that you can use them conveniently. I may revisit that decision.
- [x] Implement basic decoding/encoding from GeoJSON.
- [ ] Add a normalizing decoder, which will accept a much wider array of geojson (useful for user supplied geojson files).
- [ ] Implement a wide array of common geospatial algorithms.
- [ ] Write a whole ton of docs.
- [ ] Publish.
- [ ] Integrate with gampleman/elm-mapbox.
