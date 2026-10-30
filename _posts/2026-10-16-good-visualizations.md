---
layout: post
title:  "Good Visualizations are Rare Gems"
date:   2026-10-16 00:00:00 -0700
categories: general techniques
excerpt_separator: "<!--more-->"
---

In data analysis, good visualizations are hard to come by. Mediocre visualizations are ubiquitous, though, and from them we can learn how to improve our own technique. I'm trying to become a better visualizer, for one because it's never been easier to create elaborate charts or figures with the assistance of coding agents.<!--more--> But secondly, visualization is a key element in storytelling, and I believe that analysts ought to prioritize this more, since so many non-technical audiences will have an easier time grasping a concept through visual aid rather than through direct model interpretation. This assumes the visualization is well-designed, though.[^1]

[^1]: Background aside, I just listened to a podcast where a designer described the modern world through her eyes, articulating why it often feels drab or monotonous: <https://podcasts.apple.com/us/podcast/is-the-world-getting-uglier/id1548604447?i=1000792723814>.

Recently, I've been jogging on the treadmill for exercise, and keeping track of my heart rate while doing so. I'm sure anyone who's used wearable or heart rate tools during workouts asks the question, *What should my heart rate be?*... Well, the treadmill in my gym has an answer to that question in the form of a visualization. I'm just going to show a picture of it below and then elaborate:

![The heart rate visualization on my gym's treadmill. My heart rate at the time of taking this phot was 122 beats per minute.](/assets/images/treadmill-heart-rate-tool.jpg)

The data visualization (data "viz", for short) to which I'm referring is the graph in the center of the photo, titled, *HEART RATE ZONES*. Note the color-coded labels for *ATHLETIC CONDITIONING* (red) through to *LOW INTENSITY* (dark gray). Outside of the title, axis label, and internal labels of the chart, this viz offers very little context. I think that's the starting point for the critique, actually. I think any analyst producing a visualization such as this ought to pose the question:

> In what context will this visualization be used?

Since I was using the treadmill to workout, and since the treadmill has a heart rate monitor, I think it's safe to assume this viz would be used by joggers or runners who are curious about heart rate thresholds. Indeed, this was the case for me so I'll continue under that assumption.

Next I examined the axes and noticed the horizontal axis is representing age, in years. The vertical axis is unlabeled but the title suggests that it represents heart rate, in beats per minute (bpm). I wouldn't expect *every* user to know this, though, and so it is an area in which the viz could be improve. For two dimensional charts such as this, it's courteous to include both axes labels, even if they are obvious to the creator. If you lose someone to the axis label because they didn't share the same assumptions, you've basically lost any hope of conveying information via the visual aid.

While on the axes, the detail-oriented reader will notice that the horizontal axis 'ticks' are labelled according to vertical segments, with each segment representing an increment of five years. The vertical axis ticks are not aligned with the horizontal segments of the chart, however. This can lead to confusion; for example, where is the intersection of 40 years and 110 bpm? It's impossible to tell because of the mis-labeling. I'd fault this as a minor flaw of the viz.

But moving past the labels and the axes and onto the internal chart content, we should be able to reach some understanding of the visualization's purpose. I'm not sure the message is clear, though, and it's already required excusing several design flaws. I believe that this visualization attempts to show the relationship between age, heart rate, and cardiovascular zone for the general population so that any user can identify his or her "target" zone according to their goals. =

Let me conclude by providing two examples, labeling them on the viz itself:

![The heart rate visualization, now labelled for two hypothetical users.](/assets/images/treadmill-heart-rate-labelled.jpg)

- The user represented by the blue star is of age thirty years, targeting her workout as endurance training, and therefore striving for a heart rate of 140-160 bpm.

- The user represented by the blue hexagon is of age fifty years, targeting his workout as baseline cardio ("heart healthy"); he strives for a heart rate of 100-120 bpm.

Again, I *think* I have a grasp on this visualization but I don't know for sure, and I imagine that most people would *not* spend this much time dissecting a tool so mundane. However, because I'm thinking a lot about visualization in the broader scope of data analysis, I feel like this viz is minimally adequate in getting a point across. It's an example of a mediocre visualization, of which we have many, so I'm not claiming its exceptional for any reason. Rather, I think it offers a lot to learn from, should someone aim to design a visual aid with a similar audience in mind.