*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� New_Arm       ���ࠡ��稪 ���३ ��ᨭ                     �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                      �������� ࠡ�祣� ����.                          �
*�                                                                        �
*����������������������������������������������������������� 03.01.2004 �ͼ
PROCEDURE New_Arm

PRIVATE mss, tmpArm, s_sav, tmpNet, tmpMacro

tmpArm = ""
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  ������� ࠡ�祥 ����? [ �� ] [ ��� ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 2
  RETURN .F.
ENDIF

DO Ch_Arm WITH tmpArm

IF EMPTY(tmpArm)
  RETURN .F.
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"ARM_INFO") ORDER TAG ARM_C ALIAS ARM_4103

tmpNet = SYS(0)
tmpNet = LEFT(tmpNet, AT("#",tmpNet)-1)
IF SEEK(tmpArm)
  IF .NOT. ALLTRIM(tmpNet) == ALLTRIM(NET_NAME)
    mss(1) = ""
    mss(2) = CHR(0)+"  �� 㢥७�, �� �� ��� ࠡ�祥 ����? [ �� ] [ ��� ]"
    mss(3) = ""
    IF Out_Mess(5, "mss") = 2
      USE
      SELECT (s_sav)
      RETURN .F.
    ENDIF
    REPLACE NET_NAME WITH tmpNet
  ENDIF
ENDIF
USE
IF .NOT. Is_Dir(prmo_path)
  tmpMacro = "! MD "+LEFT(prmo_path, LEN(prmo_path)-1)
  &tmpMacro
ENDIF

IF .NOT. Is_Dir(tmpo_path)
  tmpMacro = "! MD "+LEFT(tmpo_path, LEN(tmpo_path)-1)
  &tmpMacro
ENDIF

CREATE DBF (prmo_path+"O_PARMS") (ARM_C C(3))
APPEND BLANK
REPLACE ARM_C WITH tmpArm
USE

SELECT (s_sav)

RETURN .T.