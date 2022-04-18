*&---------------------------------------------------------------------*
*& Report z_change_pw
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_change_pw.

PARAMETERS: uname       TYPE uname MATCHCODE OBJECT o2username OBLIGATORY,
            pw          TYPE char40 LOWER CASE OBLIGATORY,
            pw_rep      TYPE char40 LOWER CASE OBLIGATORY,
            iter(4)     TYPE n DEFAULT 1024 OBLIGATORY,
            salt_len(3) TYPE n DEFAULT 96 OBLIGATORY.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CHECK screen-name EQ 'PW' OR screen-name EQ 'PW_REP'.
    screen-invisible = 1.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF pw <> pw_rep.
    MESSAGE 'Die Passwörter stimmen nicht überein' TYPE 'E'.
  ENDIF.

end-of-SELECTION.

  DATA salt TYPE string.

  CALL FUNCTION 'GENERAL_GET_RANDOM_PWD'
    EXPORTING
      number_chars = CONV i( salt_len / 8 )
    IMPORTING
      random_pwd   = salt.

  DATA(digest) = cl_abap_message_digest=>get_instance( ).

  DATA(pw_hex) = digest->string_to_xstring( CONV string( pw ) ).
  DATA(salt_hex) = digest->string_to_xstring( salt ).

  DATA(combined) = pw_hex && salt_hex.

  digest->digest( EXPORTING if_data = CONV xstring( combined ) IMPORTING ef_hashxstring = DATA(result) ).

  DO iter - 1 TIMES.
    digest = digest->get_instance( ).
    digest->digest( EXPORTING if_data = CONV xstring( pw_hex && result ) IMPORTING ef_hashxstring = result ).
  ENDDO.

  DATA base64_pw_and_salt TYPE string.

  CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
    EXPORTING
      input  = CONV xstring( result && salt_hex )
    IMPORTING
      output = base64_pw_and_salt.


  DATA(final_string) = |\{x-issha, 1024\}{ base64_pw_and_salt }|.

  UPDATE usr02 SET locnt = 0,
                   pwdchgdate = 0,
                   gltgv = @( CONV d( |{ sy-datum - 1 }| ) ),
                   gltgb = '99991231',
                   pwdinitial = 2,
                   pwdlockdate = @( VALUE d( ) ),
                   uflag = 0,
                   pwdsaltedhash = @final_string,
                   codvn = 'H'
               WHERE bname = @uname.

  IF sy-subrc = 0.
    MESSAGE |Passwort für Benutzer { uname } erfolgreich geändert| TYPE 'S'.
  ELSE.
    MESSAGE |Das Passwort konnte nicht geändert werden| TYPE 'E'.
  ENDIF.
