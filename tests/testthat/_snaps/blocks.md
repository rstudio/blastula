# spacer blocks have the correct internal contents

    Code
      spacer_block
    Output
      <table class="message-block block_spacer">
        <tr>
          <td>&nbsp;</td>
        </tr>
      </table>

# article items have the correct internal contents

    Code
      an_article
    Output
      <div style="margin-bottom:12px;">
        <a href="https://en.wikipedia.org/wiki/Japan" style="text-decoration:none;"><img src="https://i.imgur.com/aYOm3Tk.jpg" width="100%" height="200" style="height:auto !important;"/></a>
      </div>
      <h3 style="margin:0;">
        <a href="https://en.wikipedia.org/wiki/Japan" style="text-decoration:none;">Japan</a>
      </h3>
      <div>Japan is an archipelago consisting of 6,852 islands along East Asia's Pacific Coast.</div>

# blocks have the correct internal contents

    Code
      block_4
    Output
      <h1 class="message-block block_title" style="color:#222222;font-weight:300;line-height:1.4;margin:0;font-size:36px;margin-bottom:4px;text-align:center;">This is a title block.</h1>
      <table class="message-block block_spacer">
        <tr>
          <td>&nbsp;</td>
        </tr>
      </table>
      <div class="message-block block_articles">
        <table class="articles" cellspacing="0" cellpadding="0" width="100%">
          <tr>
            <td class="article" valign="top" width="100%">
              <div style="margin-bottom:12px;">
                <a href="https://en.wikipedia.org/wiki/Japan" style="text-decoration:none;"><img src="https://i.imgur.com/aYOm3Tk.jpg" width="100%" height="200" style="height:auto !important;"/></a>
              </div>
              <h3 style="margin:0;">
                <a href="https://en.wikipedia.org/wiki/Japan" style="text-decoration:none;">Japan</a>
              </h3>
              <div>Japan is an archipelago consisting of 6,852 islands along East Asia's Pacific Coast.</div>
            </td>
          </tr>
        </table>
      </div>
      <div class="message-block block_text" style="text-align:left;">This is a block of text.</div>

