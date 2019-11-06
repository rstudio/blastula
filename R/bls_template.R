bls_standard_template <-
"<!doctype html>
<html>
  <head>
    <meta name=\"viewport\" content=\"width=device-width\">
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
    <title>{title}</title>
    <style media=\"all\" type=\"text/css\">
    @media only screen and (max-width: 640px) {
      .span-2,
      .span-3 {
        float: none !important;
        max-width: none !important;
        width: 100% !important;
      }
      .span-2 > table,
      .span-3 > table {
        max-width: 100% !important;
        width: 100% !important;
      }
    }

    @media all {
      .btn-primary table td:hover {
        background-color: #34495e !important;
      }
      .btn-primary a:hover {
        background-color: #34495e !important;
        border-color: #34495e !important;
      }
    }

    @media all {
      .btn-secondary a:hover {
        border-color: #34495e !important;
        color: #34495e !important;
      }
    }

    @media only screen and (max-width: 640px) {
      h1 {
        font-size: 36px !important;
        margin-bottom: 16px !important;
      }
      h2 {
        font-size: 28px !important;
        margin-bottom: 8px !important;
      }
      h3 {
        font-size: 22px !important;
        margin-bottom: 8px !important;
      }
      .main p,
      .main ul,
      .main ol,
      .main td,
      .main span {
        font-size: 16px !important;
      }
      .wrapper {
        padding: 8px !important;
      }
      .article {
        padding-left: 8px !important;
        padding-right: 8px !important;
      }
      .content {
        padding: 0 !important;
      }
      .container {
        padding: 0 !important;
        padding-top: 8px !important;
        width: 100% !important;
      }
      .header {
        margin-bottom: 8px !important;
        margin-top: 0 !important;
      }
      .main {
        border-left-width: 0 !important;
        border-radius: 0 !important;
        border-right-width: 0 !important;
      }
      .btn table {
        max-width: 100% !important;
        width: 100% !important;
      }
      .btn a {
        font-size: 16px !important;
        max-width: 100% !important;
        width: 100% !important;
      }
      .img-responsive {
        height: auto !important;
        max-width: 100% !important;
        width: auto !important;
      }
      .alert td {
        border-radius: 0 !important;
        font-size: 16px !important;
        padding-bottom: 16px !important;
        padding-left: 8px !important;
        padding-right: 8px !important;
        padding-top: 16px !important;
      }
      .receipt,
      .receipt-container {
        width: 100% !important;
      }
      .hr tr:first-of-type td,
      .hr tr:last-of-type td {
        height: 16px !important;
        line-height: 16px !important;
      }
    }

    @media all {
      .ExternalClass {
        width: 100%;
      }
      .ExternalClass,
      .ExternalClass p,
      .ExternalClass span,
      .ExternalClass font,
      .ExternalClass td,
      .ExternalClass div {
        line-height: 100%;
      }
      .apple-link a {
        color: inherit !important;
        font-family: inherit !important;
        font-size: inherit !important;
        font-weight: inherit !important;
        line-height: inherit !important;
        text-decoration: none !important;
      }
    }
    </style>

    <!--[if gte mso 9]>
    <xml>
 <o:OfficeDocumentSettings>
  <o:AllowPNG/>
  <o:PixelsPerInch>96</o:PixelsPerInch>
 </o:OfficeDocumentSettings>
</xml>
<![endif]-->
  </head>
  <body style=\"font-family: Helvetica, sans-serif; -webkit-font-smoothing: antialiased; font-size: 14px; line-height: 1.4; -ms-text-size-adjust: 100%; -webkit-text-size-adjust: 100%; background-color: #f6f6f6; margin: 0; padding: 0;\">
    <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"body\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; background-color: #f6f6f6;\" width=\"100%\" bgcolor=\"#f6f6f6\">
      <tr>
        <td style=\"font-family: Helvetica, sans-serif; font-size: 14px; color: #f6f6f6; vertical-align: top;\" valign=\"top\">&nbsp;</td>
        <td class=\"container\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; margin: 0 auto !important; max-width: 600px; padding: 0; padding-top: 24px; width: 600px;\" width=\"600\" valign=\"top\">
          <div class=\"content\" style=\"box-sizing: border-box; display: block; margin: 0 auto; max-width: 600px; padding: 0;\">

            <!-- START CENTERED WHITE CONTAINER -->
            <span class=\"preheader\" style=\"color: transparent; display: none; height: 0; max-height: 0; max-width: 0; opacity: 0; overflow: hidden; mso-hide: all; visibility: hidden; width: 0;\"></span>

            <!-- START HEADER -->
            <div class=\"header\" style=\"margin-bottom: 24px; margin-top: 0; width: 100%;\">
              <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; min-width: 100%;\" width=\"100%\">
                {html_header}
              </table>
            </div>

            <!-- END HEADER -->
            <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"main\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; background: #fff; border-radius: 4px;\" width=\"100%\">

              <!-- START MAIN CONTENT AREA -->
              <tbody>
                {html_body_text}
              <!-- END MAIN CONTENT AREA -->
              </tbody>
            </table>

            <!-- START FOOTER -->
            <div class=\"footer\" style=\"clear: both; padding-top: 24px; text-align: center; width: 100%;\">
              <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">
                {html_footer}
              </table>
            </div>
            <!-- END FOOTER -->

  <!-- END CENTERED WHITE CONTAINER --></div>
          </td>
          <td style=\"font-family: sans-serif; font-size: 14px; color: #f6f6f6; vertical-align: top;\">&nbsp;</td>
        </tr>
      </table>
    </body>
  </html>"
