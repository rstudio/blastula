library(blastula)
library(glue)

# Attribution Information available in `README-attribution.txt`

rg_sig <-
  add_image(
    "tests/manual_tests/rg_sig.jpg",
    align = "left",
    width = "10%"
  )

email <-
  compose_email(
    body = md(glue(
"
## A *message* from our President and CEO

In my discussions with employees, one thing I see consistently demonstrated \\
is this: a desire to make the world a better place. Our Mission \\
&ndash; *Decisions made easier, lives made better* &ndash; is not just a \\
tagline; it’s part of our DNA.

This is evident in our commitment and approach to sustainability. I’m always \\
inspired when I see and hear how our employees work together in support of \\
sustainability initiatives and causes that are close to their hearts.

From a company perspective, while I’m pleased with our strategic management \\
of environmental, social and governance factors, we still need to elevate and \\
prioritize the sustainability issues of the future, and be mindful of where \\
we can make the biggest impact.

Here are a few examples of the significant progress we’ve made in 2018 and \\
how we’re building on this momentum.

- We activated a sustainability governance structure led by an Executive \\
Sustainability Council consisting of six senior leaders and overseen by our \\
Board’s Corporate Governance and Nominating Committee. We also started to \\
align our ambitions and actions to the United Nations Sustainable \\
Development Goals.

- We made or helped raise $45 million in community investments and our \\
employees and agents volunteered over 66,000 hours of service to make a \\
difference in their local communities.

- We’re establishing ourselves as a global leader in sustainable finance in \\
the investment industry. We manage our real estate, timber and agricultural \\
investments to the highest sustainability standards like the LEED and BOMA \\
Best green building certifications, and the Sustainable Forestry Initiative \\
forest management certification, to name a few.

- We continue to help nearly 28 million customers live longer and healthier \\
lives, protect what matters most to them, and manage their wealth. \\
Behavioural insurance products like Manulife Vitality in Canada and John \\
Hancock Vitality in the U.S., and Manulife Move in Asia, are continually \\
breaking new ground in helping improve people’s physical wellbeing.

I encourage you to read [this report](https://www.manulife.com/content/dam/corporate/global/en/documents/pas/MFC_SR_PAS_2018.pdf) \\
to see how this comes to life. I’m excited by the journey we’re on and I \\
know that together we can make a positive difference.

{rg_sig}

Roy Gori

President and Chief Executive Officer, Manulife
")),
    footer = md("&copy; The Manufacturers Life Insurance Company")
  )

email
