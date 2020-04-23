library(blastula)

# Attribution Information available in `README-attribution.txt`

# Note: images were previously deployed to Imgur using the following calls:

# https://i.imgur.com/gpVMFcW.jpg
# add_imgur_image(
#   image = "tests/manual_tests/the-art-of-seeing-what-is-invisible-to-others.jpg",
#   client_id = ""
# )

# https://i.imgur.com/5aJawp2.jpg
# add_imgur_image(
#   image = "tests/manual_tests/surrender-or-ill-shoot.png",
#   client_id = ""
# )

# https://i.imgur.com/18fcpkZ.jpg
# add_imgur_image(
#   image = "tests/manual_tests/young-caucasian-guy-playing-a-virtual-reality-game.jpg",
#   client_id = ""
# )

# https://i.imgur.com/8sH6Ggt.jpg
# add_imgur_image(
#   image = "tests/manual_tests/your-request-is-my-command.jpg",
#   client_id = ""
# )

email <-
  compose_email(
    body =
      blocks(
        block_spacer(),
        block_title("Exciting New VR Innovations"),
        block_spacer(),
        block_text(
"VR is here to stay! New hardware offerings by major VR players are heating up
the shelves and, later, getting people moving in their living rooms!"
        ),
        block_articles(
          article(
            image = "https://i.imgur.com/18fcpkZ.jpg",
            title = "VR Combi Pack",
            content =
              "Ejecta Corporation has developed an engaging combination
               of full-featured headset with dual controllers. Software
               applications for this will be available 1-2 years from now."
          ),
          article(
            image = "https://i.imgur.com/5aJawp2.jpg",
            title = "Hyperreal",
            content =
              "By utilizing a combination of the Hyperreal headset and
               additional measures, one can have the sensation of being
               truly immersed in the VR world."
          ),
          article(
            image = "https://i.imgur.com/gpVMFcW.jpg",
            title = "Superslim",
            content =
              "VR headsets needn't be bulky monstrosities that weigh you
               down. The Superslim VR Visor prototype only weighs a few
               ounces, using a waveguide display and foveated rendering."
          )
        ),
        block_spacer(),
        block_articles(
          article(
            image = "https://i.imgur.com/8sH6Ggt.jpg",
            title = NULL,
            content =
              "Soon, we expect VR to be fashionable! Just take a look at
               this image for a glimpse of haute couture: VR Style."
          )),
        block_text(md(
"### See you next Month!
 This wraps up the May 2020 Edition of our monthly newsletter that
 showcases the latest in VR technologies. Next month, we'll provide
 info on can't miss VR and AR events in 2020."
        ))
      ),
    footer =
      blocks(
        block_text(
          md("Thanks for reading our newsletter. You can also find us here:<br>"),
          align = "center"
        ),
        block_social_links(
          social_link(
            service = "twitter",
            link = "https://twitter.com/RtoVR",
            variant = "color"
          ),
          social_link(
            service = "facebook",
            link = "http://www.facebook.com/RoadtoVR",
            variant = "color"
          ),
          social_link(
            service = "youtube",
            link = "https://www.youtube.com/user/RoadtoVR/videos",
            variant = "color"
          ),
          social_link(
            service = "rss",
            link = "https://www.roadtovr.com/feed",
            variant = "color"
          )
        ),
        block_text(
          md("All photos obtained from https://photos.icons8.com<br>"),
          align = "center"
        )
      )
  )

email
