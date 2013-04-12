*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          ”ใญชๆจ๏, ขฎงขเ ้ ๎้ ๏ เใแแชฎฅ ญ งข ญจฅ ฌฅแ๏ๆ .               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Month_R
PARAMETER n_month

DO CASE
CASE n_month = 1
  RETURN "ญข เ์"
CASE n_month = 2
  RETURN "”ฅขเ ซ์"
CASE n_month = 3
  RETURN " เโ"
CASE n_month = 4
  RETURN "€ฏเฅซ์"
CASE n_month = 5
  RETURN " ฉ"
CASE n_month = 6
  RETURN "๎ญ์"
CASE n_month = 7
  RETURN "๎ซ์"
CASE n_month = 8
  RETURN "€ขฃใแโ"
CASE n_month = 9
  RETURN "‘ฅญโ๏กเ์"
CASE n_month = 10
  RETURN "ชโ๏กเ์"
CASE n_month = 11
  RETURN "ฎ๏กเ์"
CASE n_month = 12
  RETURN "ฅช กเ์"
ENDCASE
RETURN ""
