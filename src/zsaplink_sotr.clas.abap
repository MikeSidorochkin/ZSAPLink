class ZSAPLINK_SOTR definition
  public
  inheriting from ZSAPLINK
  final
  create public .

public section.

  methods CHECKEXISTS
    redefinition .
  methods CREATEIXMLDOCFROMOBJECT
    redefinition .
  methods CREATEOBJECTFROMIXMLDOC
    redefinition .
protected section.

  methods DELETEOBJECT
    redefinition .
  methods GETOBJECTTYPE
    redefinition .
private section.
ENDCLASS.



CLASS ZSAPLINK_SOTR IMPLEMENTATION.


METHOD checkexists.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Berthe Joseph
*      joseph.berthe@gmail.com

* No implementation

  exists = abap_true.
ENDMETHOD.


method CREATEIXMLDOCFROMOBJECT.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Berthe Joseph
*      joseph.berthe@gmail.com

  TYPES: BEGIN OF totrhead,
          packet TYPE sotr_head-paket,
         END OF totrhead.

  DATA rc(4)           TYPE n.
  DATA root_node    TYPE REF TO if_ixml_element.
  DATA sub_node     TYPE REF TO if_ixml_element.
  DATA datarow_node TYPE REF TO if_ixml_element.
  DATA _concept     TYPE sotr_head-concept.
  DATA _objtype     TYPE string.
  DATA _header      TYPE sotr_head.
  DATA _entry_keys  TYPE TABLE OF sotr_text.
  DATA _entry       TYPE TABLE OF sotr_text.
  DATA _otr_keys    TYPE TABLE OF sotr_key.
  DATA _paket       TYPE totrhead.

  FIELD-SYMBOLS: <dyn_tab> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <dyn_wa>  TYPE ANY.
  FIELD-SYMBOLS: <fs_otr_key> TYPE sotr_key.

* Check that table exits.
  REFRESH _otr_keys.
  CLEAR _header.

  _paket-packet = objname.
  CALL FUNCTION 'SOTR_GET_KEYS_FOR_PACKAGE'
    EXPORTING
      paket          = _paket-packet
    IMPORTING
      sotr_keys      = _otr_keys
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
    EXPORTING
      textid = zcx_saplink=>error_message
      msg    = `OTR : Package not found`.
  ENDIF.

  _objtype  = getobjecttype( ).
  root_node = xmldoc->create_element( _objtype ).
  me->setattributesfromstructure( node = root_node structure = _paket  ).

  LOOP AT _otr_keys ASSIGNING <fs_otr_key>.
    CLEAR: _header.
    REFRESH: _entry.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = <fs_otr_key>-concept
      IMPORTING
        header         = _header
      TABLES
        entries        = _entry
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.




    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = `OTR not found`.
    ENDIF.

* Create sub node
    sub_node = xmldoc->create_element( 'SOTR_HEAD' ).
    me->setattributesfromstructure( node = sub_node structure = _header  ).

* Create dynamic internal table and work area
    ASSIGN ('_ENTRY[]') TO <dyn_tab>.

* Write records to XML node
    LOOP AT <dyn_tab> ASSIGNING <dyn_wa>.
      datarow_node = xmldoc->create_element( `SOTR_TEXT` ).
      me->setattributesfromstructure( node = datarow_node structure = <dyn_wa> ).
      rc = sub_node->append_child( datarow_node ).
    ENDLOOP.

    rc = root_node->append_child( sub_node ).
  ENDLOOP.
* Add node
  rc = xmldoc->append_child( root_node ).

  ixmldocument = xmldoc.

endmethod.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Berthe Joseph
*      joseph.berthe@gmail.com

  TYPES: BEGIN OF totrhead,
    packet TYPE sotr_head-paket,
    END OF totrhead.


  DATA root_node        TYPE REF TO if_ixml_element.
  DATA sub_node     TYPE REF TO if_ixml_element.
  DATA sub_node_node     TYPE REF TO if_ixml_element.
  DATA sub_node_filter   TYPE REF TO if_ixml_node_filter.
  DATA sub_node_iterator TYPE REF TO if_ixml_node_iterator.

  DATA datarow_node     TYPE REF TO if_ixml_element.
  DATA datarow_filter   TYPE REF TO if_ixml_node_filter.
  DATA datarow_iterator TYPE REF TO if_ixml_node_iterator.

  DATA _objtype           TYPE string.


  DATA _header      TYPE sotr_head.
  DATA _text_tab    TYPE sotr_text_tt.
  DATA _packet      TYPE sotr_pack.
  DATA _object_type TYPE trobjtype.
  DATA _msg_err     TYPE string.
  DATA _paket       TYPE totrhead.
  DATA checkexists   TYPE flag.

  DATA dref_tab_head TYPE REF TO data.
  DATA dref_head  TYPE REF TO data.
  DATA dref_tab_text TYPE REF TO data.
  DATA dref_text  TYPE REF TO data.

  FIELD-SYMBOLS: <dyn_tab_head>  TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <dyn_tab_text>  TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <dyn_head>   TYPE sotr_head .
  FIELD-SYMBOLS: <dyn_text>   TYPE sotr_text .


  " -->
  " This bloc is useless for this version.
  checkexists = checkexists( ).
  IF checkexists IS NOT INITIAL.
    IF overwrite IS INITIAL.
      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>existing.
    ELSE.
*     delete object for new install
      deleteobject( ).
    ENDIF.
  ENDIF.
  " <--

  _objtype = getobjecttype( ).
  _object_type = _objtype.
  xmldoc = ixmldocument.
  root_node ?= xmldoc->find_from_name( _objtype ).

* Get table name from XML.
  me->getstructurefromattributes(
          EXPORTING  node      = root_node
          CHANGING   structure = _paket ).

  _packet = _paket-packet.

  " Check if Package exist.
  SELECT COUNT(*) FROM tdevc WHERE devclass EQ _packet.

  IF sy-subrc EQ 0.

* Create dynamic internal table and work area
    CREATE DATA dref_tab_head TYPE TABLE OF ('SOTR_HEAD').
    ASSIGN dref_tab_head->* TO <dyn_tab_head>.
    CREATE DATA dref_head LIKE LINE OF <dyn_tab_head>.
    ASSIGN dref_head->* TO <dyn_head>.

* Build dynamic internal table from XML
    FREE: sub_node_filter, sub_node_iterator, sub_node_node.

    " Find all sub_node.

    sub_node_filter = xmldoc->create_filter_name( `SOTR_HEAD` ).
    sub_node_iterator = xmldoc->create_iterator_filtered( sub_node_filter ).
    sub_node_node ?= sub_node_iterator->get_next( ).
    WHILE sub_node_node IS NOT INITIAL.
      APPEND INITIAL LINE TO <dyn_tab_head> ASSIGNING <dyn_head>.
      me->getstructurefromattributes(
              EXPORTING   node      = sub_node_node
              CHANGING    structure = <dyn_head> ).


* Create dynamic internal table and work area
      CREATE DATA dref_tab_text TYPE TABLE OF ('SOTR_TEXT').
      ASSIGN dref_tab_text->* TO <dyn_tab_text>.
      CREATE DATA dref_text LIKE LINE OF <dyn_tab_text>.
      ASSIGN dref_text->* TO <dyn_text>.
      FREE: datarow_filter, datarow_iterator, datarow_node.

      datarow_filter = sub_node_node->create_filter_name( `SOTR_TEXT` ).
      datarow_iterator = sub_node_node->create_iterator_filtered( datarow_filter ).
      datarow_node ?= datarow_iterator->get_next( ).
      WHILE datarow_node IS NOT INITIAL.
        APPEND INITIAL LINE TO <dyn_tab_text> ASSIGNING <dyn_text>.
        me->getstructurefromattributes(
        EXPORTING   node      = datarow_node
        CHANGING    structure = <dyn_text> ).

        APPEND <dyn_text> TO _text_tab.


        datarow_node ?= datarow_iterator->get_next( ).
      ENDWHILE.

* Create the OTR
      CALL FUNCTION 'SOTR_CREATE_CONCEPT'
        EXPORTING
          paket                         = _packet
          crea_lan                      = <dyn_head>-crea_lan
          alias_name                    = <dyn_head>-alias_name
          object                        = 'WDCC'
          entries                       = _text_tab
        EXCEPTIONS
          package_missing               = 1
          crea_lan_missing              = 2
          object_missing                = 3
          paket_does_not_exist          = 4
          alias_already_exist           = 5
          object_type_not_found         = 6
          langu_missing                 = 7
          identical_context_not_allowed = 8
          text_too_long                 = 9
          error_in_update               = 10
          no_master_langu               = 11
          error_in_concept_id           = 12
          alias_not_allowed             = 13
          tadir_entry_creation_failed   = 14
          internal_error                = 15
          error_in_correction           = 16
          user_cancelled                = 17
          no_entry_found                = 18
          OTHERS                        = 19.
      IF sy-subrc <> 0.
        DATA: lv_msg  TYPE string.
        CASE sy-subrc.
          WHEN 1.
            lv_msg = 'OTR : Package missing.'.
          WHEN 2.
            lv_msg = 'OTR : Language missing.'.
          WHEN 3.
            lv_msg = 'OTR : Object missing.'.
          WHEN 4.
            lv_msg = 'OTR : Packet doesn''t exist.'.
          WHEN 5.
            lv_msg = 'OTR : Alias already exist.'.
          WHEN 6.
            lv_msg = 'OTR : Object type not found.'.
          WHEN 7.
            lv_msg = 'OTR : Language missing.'.
          WHEN 8.
            lv_msg = 'OTR : Identical context not allowed.'.
          WHEN 9.
            lv_msg = 'OTR : Text too long.'.
          WHEN 10.
            lv_msg = 'OTR : Error in update.'.
          WHEN 11.
            lv_msg = 'OTR : No master language.'.
          WHEN 12.
            lv_msg = 'OTR : Error in concept ID (GUID).'.
          WHEN 13.
            lv_msg = 'OTR : Alias not allowed.'.
          WHEN 14.
            lv_msg = 'OTR : TADIR entry creation failed.'.
          WHEN 15.
            lv_msg = 'OTR : Internal Error.'.
          WHEN 16.
            lv_msg = 'OTR : Error in correction.'.
          WHEN 17.
            lv_msg = 'OTR : User cancelled.'.
          WHEN 18.
            lv_msg = 'OTR : No entry found.'.
          WHEN OTHERS.
            lv_msg = 'OTR : Global error.'.
        ENDCASE.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg    = lv_msg.
      ENDIF.

      sub_node_node ?= sub_node_iterator->get_next( ).
    ENDWHILE.
  ELSE.

    CONCATENATE 'Package ' _packet ' doesn''t exist.' INTO _msg_err RESPECTING BLANKS.

    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = _msg_err.

  ENDIF.


  name = _packet.

ENDMETHOD.


METHOD deleteobject.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Berthe Joseph
*      joseph.berthe@gmail.com


  TYPES: BEGIN OF totrhead,
        packet TYPE sotr_head-paket,
  END OF totrhead.

  DATA _concept     TYPE sotr_head-concept.
  DATA _objtype     TYPE string.
  DATA _header      TYPE sotr_head.
  DATA _entry_keys  TYPE TABLE OF sotr_text.
  DATA _entry       TYPE TABLE OF sotr_text.
  DATA _otr_keys    TYPE TABLE OF sotr_key.
  DATA _paket       TYPE totrhead.

  FIELD-SYMBOLS: <fs_otr_key> TYPE sotr_key.

  _paket-packet = objname.

  CALL FUNCTION 'SOTR_GET_KEYS_FOR_PACKAGE'
    EXPORTING
      paket          = _paket-packet
    IMPORTING
      sotr_keys      = _otr_keys
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg    = `OTR : Package not found`.
  ENDIF.

  _objtype  = getobjecttype( ).

  LOOP AT _otr_keys ASSIGNING <fs_otr_key>.
    CLEAR: _header.
    REFRESH: _entry.

    CALL FUNCTION 'SOTR_DELETE_CONCEPT'
      EXPORTING
        concept             = <fs_otr_key>-concept
      EXCEPTIONS
        no_authorization    = 1
        no_entry_found      = 2
        concept_used        = 3
        no_master_language  = 4
        no_source_system    = 5
        no_tadir_entry      = 6
        error_in_correction = 7
        user_cancelled      = 8
        text_not_found      = 9
        invalid_package     = 10
        text_not_changeable = 11
        text_enqueued       = 12
        no_correction       = 13
        parameter_error     = 14.

    DATA _msg_err     TYPE string.
    CLEAR _msg_err.

    CASE sy-subrc.
      WHEN 14.
        _msg_err = 'OTR (Del) : No TADIR found.'.
      WHEN 7.
        _msg_err = 'OTR (Del) : Error in correction.'.
      WHEN 8.
        _msg_err = 'OTR (Del) : ser Cancelled.'.
      WHEN 10.
        _msg_err = 'OTR (Del) : Invalid package.'.
      WHEN 14.
        _msg_err = 'OTR (Del) : ext no t changeable.'.
      WHEN 12.
        _msg_err = 'OTR (Del) : Txt Enqueued.'.
      WHEN 13.
        _msg_err = 'OTR (Del) : No Correction'.
      WHEN 14.
        _msg_err = 'OTR (Del) : Parameter error.'.
      WHEN 9.
      WHEN OTHERS.
    ENDCASE.

    IF _msg_err IS NOT INITIAL.
      CONCATENATE _msg_err <fs_otr_key>-concept INTO _msg_err SEPARATED BY space.

      RAISE EXCEPTION TYPE zcx_saplink
        EXPORTING
          textid = zcx_saplink=>error_message
          msg    = _msg_err.
    ENDIF.


  ENDLOOP.
ENDMETHOD.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This file is part of SAPlink.                                     |
*|                                                                     |
*|   SAPlink is free software; you can redistribute it and/or modify   |
*|   it under the terms of the GNU General Public License as published |
*|   by the Free Software Foundation; either version 2 of the License, |
*|   or (at your option) any later version.                            |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*|   You should have received a copy of the GNU General Public License |
*|   along with SAPlink; if not, write to the                          |
*|   Free Software Foundation, Inc.,                                   |
*|   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA          |
*\---------------------------------------------------------------------/

*      Plugin created by:
*      Berthe Joseph
*      joseph.berthe@gmail.com

  objecttype = 'SOTR'.  "Table Contents

endmethod.
ENDCLASS.
