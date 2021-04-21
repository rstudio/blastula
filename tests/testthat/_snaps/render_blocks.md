# the `render_block_title()` function returns the expected output

    Code
      block_title
    Output
      <h1 class="message-block block_title" style="color:#222222;font-weight:300;line-height:1.4;margin:0;font-size:36px;margin-bottom:4px;text-align:center;">Test Title</h1>

# the `render_block_text()` function returns the expected output

    Code
      block_text
    Output
      <div class="message-block block_text" style="text-align:left;">Test Text</div>

# the `render_block_articles()` function returns the expected output

    Code
      block_articles_1
    Output
      <div class="message-block block_articles">
        <table class="articles" cellspacing="0" cellpadding="0" width="100%">
          <tr>
            <td class="article" valign="top" width="100%">
              <h3 style="margin:0;">
                <a href="link_1" style="text-decoration:none;">title_1</a>
              </h3>
              <div>content_1</div>
            </td>
          </tr>
        </table>
      </div>

---

    Code
      block_articles_2
    Output
      <div class="message-block block_articles">
        <table class="articles" cellspacing="0" cellpadding="0" width="100%">
          <tr>
            <td class="article" valign="top" width="50%">
              <h3 style="margin:0;">
                <a href="link_1" style="text-decoration:none;">title_1</a>
              </h3>
              <div>content_1</div>
            </td>
            <td width="1">
              <img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAADklEQVQYlWNgGAWDEwAAAZoAARbK02kAAAAASUVORK5CYII=" width="12" height="12" style="width:12px !important;height:12px;max-width:12px !important;min-width:12px !important;"/>
            </td>
            <td class="article" valign="top" width="50%">
              <h3 style="margin:0;">
                <a href="link_2" style="text-decoration:none;">title_2</a>
              </h3>
              <div>content_2</div>
            </td>
          </tr>
        </table>
      </div>

---

    Code
      block_articles_3
    Output
      <div class="message-block block_articles">
        <table class="articles" cellspacing="0" cellpadding="0" width="100%">
          <tr>
            <td class="article" valign="top" width="33%">
              <h3 style="margin:0;">
                <a href="link_1" style="text-decoration:none;">title_1</a>
              </h3>
              <div>content_1</div>
            </td>
            <td width="1">
              <img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAADklEQVQYlWNgGAWDEwAAAZoAARbK02kAAAAASUVORK5CYII=" width="12" height="12" style="width:12px !important;height:12px;max-width:12px !important;min-width:12px !important;"/>
            </td>
            <td class="article" valign="top" width="33%">
              <h3 style="margin:0;">
                <a href="link_2" style="text-decoration:none;">title_2</a>
              </h3>
              <div>content_2</div>
            </td>
            <td width="1">
              <img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAADklEQVQYlWNgGAWDEwAAAZoAARbK02kAAAAASUVORK5CYII=" width="12" height="12" style="width:12px !important;height:12px;max-width:12px !important;min-width:12px !important;"/>
            </td>
            <td class="article" valign="top" width="33%">
              <h3 style="margin:0;">
                <a href="link_3" style="text-decoration:none;">title_3</a>
              </h3>
              <div>content_3</div>
            </td>
          </tr>
        </table>
      </div>

# the social link functions work correctly

    Code
      social_link_1
    Output
      <a href="https://dribbble.com"><img src="https://raw.githubusercontent.com/rstudio/blastula/master/inst/social_icons/dribbble-bw.png" alt="dribbble" width="44" height="44" style="border: none;"/></a>

