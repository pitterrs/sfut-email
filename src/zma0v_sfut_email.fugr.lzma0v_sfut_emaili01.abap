*----------------------------------------------------------------------*
***INCLUDE LZMA0V_SFUT_EMAILI01.
*----------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  IF sy-ucomm = 'BODY'. " Maintain E-mail body
    PERFORM set_email_body.
  ENDIF.

  CLEAR: sy-ucomm.

ENDMODULE.
