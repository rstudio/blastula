library(blastula)
library(glue)

# Attribution Information
#
# Uses 'free-to-use' photos from https://photos.icons8.com (conditional on linking back);
# more information here: https://icons8.com/license


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


email <-
  compose_email(
    body =
      blocks(
        block_title("Exciting New VR Innovations"),
        block_spacer(),
        block_text(paste0(
"VR is here to stay! New hardware offerings by major VR players are heating up ",
"the shelves and, later, getting people moving in their living rooms!")),
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
              "By utilizing a combination of the Hyperreal headset (and
               additional measures), one can have the sensation of being
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
        )
      )
  )

email
