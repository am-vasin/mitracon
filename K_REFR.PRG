*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  K_Refr        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       กญฎขซฅญจฅ ชใเแ  คฎซซ เ .                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 30.03.1999 ออผ
PROCEDURE K_Refr

PRIVATE s_sav
s_sav = SELECT()
SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CRS_9330 AGAIN
m.kurs   = Get_Curs("CURS","CRS_9330")
m.kurs_b = Get_Curs("CURS_B","CRS_9330")
m.kurs_3 = Get_Curs("CURS_N","CRS_9330")
m.kurs_mmvb = Get_Curs("CURS_MMVB","CRS_9330")
USE
SELECT (s_sav)
RETURN
