*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          ��ぃ�皰→� え爛�皰爬� か� �ムイ�腮/�爬ガ�.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_TD
PRIVATE pth                    && ����Ж皚�� え爛�皰爬�
PRIVATE w, i                   && ����

*
*   蹍▲瑙ガ ����腮� 矗��Ж皚�� え爛�皰爬� �, メ�� ��ぎ, 甌Г�ガ...
*
pth = tmpo_path+"transmit"

IF .NOT. Is_Dir(pth)
  w = "! MD "+pth
  &w
ENDIF
*
*    �腮��ガ...
*
pth = pth+"\"
RELEASE w
IF ADIR(w,pth+"*.*") # 0
  FOR i = 1 TO ALEN(w,1)
    DELETE FILE (pth+w(i,1))
  ENDFOR
ENDIF

RETURN
