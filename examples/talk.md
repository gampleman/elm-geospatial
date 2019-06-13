Hello everyone!

My name is Jakub Hampl and I'm very glad to be here at Elm Europe again. It's an awesome conference.

My one claim to fame in the Elm community is my work on the gampleman/elm-visualization library. In case you haven't seen it before, it's a data visualization library aimed at building custom data graphics - charts, diagrams or even novel ways to control data in an application. And this library is still an ongoing work in progress with features small and great planned for release in the upcoming months. But, at the beginning of that project I was scoping out what I wanted to include for sure, what would be nice to haves and what was out of scope. And pretty early on I decided not to include any tools for geographic data visualization. This was for two reasons: firstly, I didn't need it at all at the time. I was working at a company that dealt with money and cloud computing resources - both which are highly abstract. But more importantly, I didn't understand most of it. Making maps looked really complicated.

But life goes in funny ways. Not long after that I switched jobs and started working for my current employer - Astrosat. We deal primarily with satellite data. Turns out, satellite data particularly lends itself to being shown on maps.

And so here I was building mapping applications. The first greenfield project I started working on we decided to use Elm -- for it's many benefits as all of you know. But when it came time to start building the geographic features of the application, we followed the advice at the time and used ports for that. And our application reached our MVP goals and we celebrated some success. But as we were building up our application our JavaScript port integration file started to get bigger and messier. Code complexity grew and it seemed to evade good structure leading to something reminiscent of jQuery scripts of old. Worse, our business logic ended up split between the two -- and unifying it became a matter of developer discipline. Which, under tight deadlines and shifting teams becomes highly unreliable.

Anyway, due to external factors the project was put on hold for about a year. In that time I had a lot of experience building mapping applications with React. But I knew our Elm project was going to be restarted and we would need a good answer to these challenges. And so I started work on building some proper infrastructure for building type safe, reliable, performant and ergonomic maps. Today I would like to share with you some of the fruits of that labor by introducing two open source packages. One of them, gampleman/elm-mapbox is designed to render high-quality maps with custom data. The other, gampleman/elm-geospatial is designed to be a toolkit for geo-spatial computing. These two tasks are highly related, but one may use the packages independently. For example, many front-end mapping applications don't need to deal with geographical data directly and instead can simply setup the mapping layer to communicate with an appropriate server. As such , and application like that only cares about the necessary metadata, but doesn't need to perform geospatial computation. On the other hand, an application may perform geographical querying, but display the results in other ways than by drawing maps.

I think a lot of this may sound quite abstract to many of you, so I will try to show you how you might use some of this with an example. This is a real world example for visualizing data related to flooding in Malaysia. In this project, we wanted to show our users data collected by a number of ground stations that are scattered around the country. There are three types of ground station that are related to flooding: rain stations, water level stations and combined stations. Rain stations are essentially buckets with a hole on the bottom that measure how much water is in them providing a view of how much rain is falling at a particular location. Water level stations are sticks in the river detecting how high the water in a river is. Combined stations perform both of these functions.

In this project we are getting a live data feed from one of our partners. So let's go ahead and visualize this data so that decision makers can evaluate the risk of a flood.

Since elm-mapbox relies on additional JavaScript support (it uses both custom elements and ports), we need to setup our project to support both JavaScript and Elm easily. The simplest way to go about it is to use Parcel, since it supports both modern JS and Elm out of the box with zero configuration. In this project, I've already setup Parcel, but it's super easy. Then, I want to install both packages. So I will run `elm install gampleman/elm-geospatial`, `elm install gampleman/elm-mapbox` and `elm install elm/http` `elm install elm/json` for good measure. Then we'll run `npm install --save elm-mapbox` to get the JS companion components. We'll go to our `index.js` and add the following code:

This is the setup code we need to get the JavaScript to communicate with Elm. We've commented out the ports setup, as we won't need it in this project.

Next, we'll copy the Light style from the gampleman/elm-mapbox repo, and add some small modifications to it to allow us to insert our own custom sources and layers.

Next, we'll create our Main.elm.

Particularly of note is the `buildStyle` function, as this is where most of the action takes place so to say. The map style in mapbox is a bit like the DOM. It's a single data structure that declaratively describes what is going to be rendered on the map. It consists of a bunch of settings, sources and layers. We have the function `light` that describes a pre-built map suitable for data visualization. It accepts additional sources and layers as parameters returning a fully built style.

Sources in Mapbox describe to the map where it should find the data to use for drawing the various shapes on the map. In themselves, sources don't cause anything to show up on the screen. This is done via layers, which describe what visual features the data coming in should have. Layers can also have filters attached to them allowing them to only target some of the data from the source. Hence any source can have a whole bunch of layers attached to it.

So in our case we add a GeoJSON source from a remote URL (which our partners provided us). This is the data we'll be working with for the rest of this example. For now, we can simply hand over the URL to the map and let the map fetch the data on its own. We don't need to worry about it.

Next, we'll create a `circle` layer and give it the ID of our source. A circle layer will show a circle at each point in the data. By default, these will be 5px radius black circles. We can change the color, radius and other visual properties. But even better, we can vary these properties based on the data being rendered. Mapbox supports a serializable expression language that allows us to encode the logic for styling. Let's look at the radius code first. We can fetch the station type property for each point and then call `matchesStr`. You can think of that like a case statement, where we compare the input with a series of options and choose output based on which matches. The last value is the default. We pass this then to the `circleRadius` property. So in this example, we have given water level stations a radius of 1, rain fall stations a radius of 3, and combined stations a radius of 5.

We do a similar trick for the color. However, here we find the first sign of trouble. The logic for figuring out the warning level message for each station is pretty complicated and messy. Here we find the first non-null property and then turn it into a color. This works, but is probably not entirely correct. Also our styling logic starts getting pretty complicated.

The good news is we've made our first map visualization!

However, our first attempt leaves a lot to be desired. First of all, the map is a bit of mess of overlapping circles and its hard to get a correct overview of the whole country. We could have high alert circles behind inactive stations and never notice. Second, the two kinds of data - rainfall and river level are hard to tell apart, but they have different characteristics for predicting a flood. Third, we are showing plenty of irrelevant data here -- you don't care about the stations that are off if you're trying to predict a flood. Finally, our code mixes styling logic with logic for dealing with messy data.

Let's make an another attempt to solve these problems. First, we're going to use the HTTP library to fetch our data. We're going to use `expectJson` to decode it. Now elm-geospatial gives us a decoder for decoding GeoJSON - but we need to give it a decoder to decode the non-geospatial part of the data, that is the JSON inside the GeoJSON. This is quite nice, since it will allow us to deal with the messy data and get nice Elm types out of it.

[code here]

Here we make a few decoders in their specific modules. We then decode and now we can split our feature collection into two - one for rainfall data and one for water levels. We can solve the problem of having too much irrelevant data by further filtering out all the stations that are turned off. Now we would like to do something interesting with the rainfall data. We will want to turn it from points into an area - giving us an understanding what the rain might look like in any part of the country.

Now there is a multitude of ways to do it - from bilinear interpolation, to very clever weather modeling.

However, for this use case we don't want anything clever. We just want to show the status of the nearest station. For this we're going to use a Voronoi diagram.

You will have to excuse a small aside that I would like to make and explain what a Voronoi diagram is. Imagine we have a surface and we put on some points, which we will call sites. Now for every point on the surface, one of three conditions can take place.

1. It is closest to one of the sites. In this case the point will be part of a face of the resulting diagram.
2. It is the closest to exactly two sites. In this case the point will be part of an edge of the resulting diagram.
3. It is closest to exactly three sites - it will be a vertex of the resulting diagram.

Once we do this for every point (of which there are infinitely many) on this infinite plane, we have a diagram consisting of a bunch of polygons dividing the plane,
such that everything inside each polygon is closest to the respective site.

Boom!

Let's apply that to our map.

Well, it looks "interesting". But our users might be under the impression we are showing them abstract art rather than a map. In order to fix that, we will need to turn it back into something that looks like Malaysia. In order to do that, we'll need the shape of Malaysia. For various reasons I won't get into, the shape used in our map isn't really suitable for the task, so we'll download another one and handily decode it just like before (we don't need any properties in this case, so just have a unit decoder).

Now we need a way to combine this shape with what we got. We're going to use polygon intersections to do this. Polygon intersections are just like set intersections for polygons - when we intersect two polygons, we get a polygon that both of the inputs covered. We can do that here, and hey presto, we now have a fairly attractive map showing us an estimate of rainfall across the country.

We can go further, for example in the prototype I generated a circle with a certain radius from each rainfall station and ran another intersection to limit the problem of making these areas unrealistically big.

Now you can be the judges of whether this is a particularly good way to visualize this particular dataset (and in our case we never ended up shipping this), but I think it illustrates nicely that one can achieve useful results by creatively combining the tools I've been demonstrating.



And so I invite you to dive into the world of web maps and discover this fascinating world on the intersection of front-end engineering, geometry and big data that has the potential to make us understand many of the problems of our world in the ancient human way - by drawing pictures of them. Thank you very much, any questions?
