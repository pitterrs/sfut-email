*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12/23/2020 at 07:44:30
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMALV_SFUT_RECV.................................*
TABLES: ZMALV_SFUT_RECV, *ZMALV_SFUT_RECV. "view work areas
CONTROLS: TCTRL_ZMALV_SFUT_RECV
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZMALV_SFUT_RECV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMALV_SFUT_RECV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMALV_SFUT_RECV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMALV_SFUT_RECV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMALV_SFUT_RECV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMALV_SFUT_RECV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMALV_SFUT_RECV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMALV_SFUT_RECV_TOTAL.

*.........table declarations:.................................*
TABLES: LFA1                           .
TABLES: T001W                          .
TABLES: T024E                          .
TABLES: ZMAL_SFUT_EMAIL                .
TABLES: ZMAL_SFUT_RECV                 .
