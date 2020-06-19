class ZSAPLINK_DEVC definition
  public
  inheriting from ZSAPLINK
  create public .

public section.
  type-pools ABAP .
  type-pools SEOP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  type-pools SEOX .

  types:
    BEGIN OF ts_field_map,
        st_fn TYPE string, " Structure Field Name
        obj_fn TYPE string, " Object Field NAME
      END OF ts_field_map .
  types:
    tt_field_map TYPE SORTED TABLE OF ts_field_map WITH UNIQUE KEY st_fn .

  class-methods CLASS_CONSTRUCTOR .
  class-methods OBJ_2_STRUCT
    importing
      !OBJ type ref to OBJECT
      !MAPPING type TT_FIELD_MAP optional
    changing
      !STRUCT type ANY .

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

  data OBJ type ref to IF_PACKAGE .
  class-data SCOMPKDTLN_MAPPING type TT_FIELD_MAP .

  methods LOAD_OBJ .
ENDCLASS.



CLASS ZSAPLINK_DEVC IMPLEMENTATION.


method CHECKEXISTS.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/


data _key type DEVCLASS.
data _devc type ref to IF_PACKAGE. "for debug

  _key = objname.
  CALL METHOD cl_package=>if_package~load_package
    EXPORTING
      i_package_name             = _key
      i_force_reload             = 'X'
    IMPORTING
      e_package                  = _devc
    EXCEPTIONS
      object_not_existing        = 1
*      unexpected_error           = 2  => DUMP
*      intern_err                 = 3  => DUMP
      object_locked_and_modified = 0
*      others                     = 5  => DUMP
          .
  IF sy-subrc = 0.
    exists = 'X'.
  ENDIF.

endmethod.


method CLASS_CONSTRUCTOR.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/

  data: _wa like LINE OF SCOMPKDTLN_MAPPING.

* FROM method M_LOAD_DATA of CL_PACKAGE

* Package
  _wa-st_fn = 'DEVCLASS'.
  _wa-obj_fn = 'PACKAGE_NAME'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Short Description of Repository Objects
  _wa-st_fn = 'CTEXT'.
  _wa-obj_fn = 'SHORT_TEXT'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Indicator for Object Deleted from Memory
  _wa-st_fn = 'DEL_IN_MEM'.
  _wa-obj_fn = 'DELETED_IN_MEMORY'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Language Key
  _wa-st_fn = 'LANGUAGE'.
  _wa-obj_fn = 'TEXT_LANGUAGE'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Original Language in Repository objects
  _wa-st_fn = 'MASTERLANG'.
  _wa-obj_fn = 'MASTER_LANGUAGE'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Changes Recorded
  _wa-st_fn = 'KORRFLAG'.
  _wa-obj_fn = 'WBO_KORR_FLAG'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* User Responsible for Package
  _wa-st_fn = 'AS4USER'.
  _wa-obj_fn = 'AUTHOR'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Transport Layer in ABAP Workbench
  _wa-st_fn = 'PDEVCLASS'.
  _wa-obj_fn = 'TRANSPORT_LAYER'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

** Short Text for Transport Layer
*  _wa-st_fn = 'LAYER_TEXT'.
*  _wa-obj_fn = 'TRANSPORT_LAYER_TEXT'.
*  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Software Component
  _wa-st_fn = 'DLVUNIT'.
  _wa-obj_fn = 'SOFTWARE_COMPONENT'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Application Component ID
  _wa-st_fn = 'COMP_POSID'.
  _wa-obj_fn = 'APPLICATION_COMPONENT_ABBREV'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Application Component
  _wa-st_fn = 'COMPONENT'.
  _wa-obj_fn = 'APPLICATION_COMPONENT'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Flag: Inherit Use Accesses from Surrounding Package
  _wa-st_fn = 'PERMINHER'.
  _wa-obj_fn = 'INHERITS_PERMISSIONS'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Surrounding Package
  _wa-st_fn = 'PARENTCL'.
  _wa-obj_fn = 'SUPER_PACKAGE_NAME'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Prefix for Package Interfaces (Suggested Value)
  _wa-st_fn = 'INTFPREFX'.
  _wa-obj_fn = 'INTERFACE_PREFIX'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Package Type (Permitted Object Types)
  _wa-st_fn = 'PACKTYPE'.
  _wa-obj_fn = 'PACKAGE_TYPE'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Constraint on Extending Package
  _wa-st_fn = 'RESTRICTED'.
  _wa-obj_fn = 'EXTENSION_RESTRICTED'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Main Package Indicator
  _wa-st_fn = 'MAINPACK'.
  _wa-obj_fn = 'MAIN_PACKAGE'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Flag for Package Check as Server
  _wa-st_fn = 'SRV_CHECK'.
  _wa-obj_fn = 'CHECKS_AS_SERVER_ENFORCED'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Flag: Package Check as Client
  _wa-st_fn = 'CLI_CHECK'.
  _wa-obj_fn = 'CHECKS_AS_CLIENT_ENFORCED'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

* Alias for R/3 Enterprise Extension in Transaction FIBF
  _wa-st_fn = 'EXT_ALIAS'.
  _wa-obj_fn = 'R3_EXTENSION_ALIAS'.
  INSERT _wa into TABLE SCOMPKDTLN_MAPPING.

endmethod.


METHOD createixmldocfromobject.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/


  DATA _devc_data TYPE scompkdtln.
  DATA rc         TYPE sysubrc.  " for debug
  DATA _objtype   TYPE string.
  DATA tmp_str    TYPE string.

  DATA root_node     TYPE REF TO if_ixml_element.

  _objtype  = getobjecttype( ).
  root_node = xmldoc->create_element( _objtype ).

  load_obj( ).
  obj_2_struct( EXPORTING obj = obj
                      mapping = scompkdtln_mapping
              CHANGING struct = _devc_data ).

  setattributesfromstructure( node = root_node structure = _devc_data  ).
* because of the following code in the previous method we have to set back DEVCLASS
*        when 'DEVCLASS'. "development class should always be $TMP
*          sValue = '$TMP'.
  tmp_str = _devc_data-devclass.
  rc = root_node->set_attribute( name = 'DEVCLASS' value = tmp_str ).

  rc = xmldoc->append_child( root_node ).
  ixmldocument = xmldoc.
ENDMETHOD.


METHOD createobjectfromixmldoc.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/

  DATA root_node        TYPE REF TO if_ixml_element.
  DATA rootnode TYPE REF TO if_ixml_element.
  DATA objname TYPE e071-obj_name.
  DATA _objtype TYPE string.
  DATA _devc_data TYPE scompkdtln.

  _objtype = getobjecttype( ).

  xmldoc = ixmldocument.
  root_node = xmldoc->find_from_name( _objtype ).

* Get Root
  me->getstructurefromattributes(
          EXPORTING  node      = root_node
          CHANGING   structure = _devc_data ).

  objname = _devc_data-devclass.

* Done in method S_LOCK_DB de CL_PACKAGE
*  CALL FUNCTION 'RS_ACCESS_PERMISSION'
*    EXPORTING
*      global_lock              = 'X'
*      mode                     = 'INSERT'
*      object                   = objname
*      object_class             = _objtype
*    EXCEPTIONS
*      canceled_in_corr         = 1
*      enqueued_by_user         = 3
*      enqueue_system_failure   = 4
*      locked_by_author         = 5
*      illegal_parameter_values = 6
*      no_modify_permission     = 7
*      no_show_permission       = 8
*      permission_failure       = 9.
*
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN 7 OR 8 OR 9.
*        RAISE EXCEPTION TYPE zcx_saplink
*          EXPORTING
*            textid = zcx_saplink=>not_authorized.
*      WHEN 5.
*        RAISE EXCEPTION TYPE zcx_saplink
*          EXPORTING
*            textid = zcx_saplink=>error_message
*            msg = 'object locked'.
*      WHEN OTHERS.
*        RAISE EXCEPTION TYPE zcx_saplink
*          EXPORTING
*            textid = zcx_saplink=>system_error.
*    ENDCASE.
*  ENDIF.

*  CALL FUNCTION 'RS_CORR_INSERT'
*    EXPORTING
*      object              = objname
*      object_class        = _objtype
*      mode                = 'INSERT'
*      global_lock         = 'X'
**     devclass            = devclass
*      author              = sy-uname
*      master_language     = sy-langu
*    EXCEPTIONS
*      cancelled           = 1
*      permission_failure  = 2
*      unknown_objectclass = 3.
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN 2.
*        RAISE EXCEPTION TYPE zcx_saplink
*          EXPORTING
*            textid = zcx_saplink=>not_authorized.
*      WHEN OTHERS.
*        RAISE EXCEPTION TYPE zcx_saplink
*          EXPORTING
*            textid = zcx_saplink=>system_error.
*    ENDCASE.
*  ENDIF.

  CALL METHOD cl_package=>if_package~create_new_package
*    EXPORTING
*      i_reuse_deleted_object     = 'X'
    IMPORTING
      e_package                  = obj
    CHANGING
      c_package_data             = _devc_data
    EXCEPTIONS
      object_already_existing    = 1
      object_just_created        = 2
      not_authorized             = 3
      wrong_name_prefix          = 4
      undefined_name             = 5
      reserved_local_name        = 6
      invalid_package_name       = 7
      short_text_missing         = 8
      software_component_invalid = 9
      layer_invalid              = 10
      author_not_existing        = 11
      component_not_existing     = 12
      component_missing          = 13
      prefix_in_use              = 14
      unexpected_error           = 15
      intern_err                 = 16
      OTHERS                     = 17
          .
  IF sy-subrc = 11.
    _devc_data-as4user = sy-uname.
    CALL METHOD cl_package=>if_package~create_new_package
*    EXPORTING
*      i_reuse_deleted_object     = 'X'
      IMPORTING
        e_package                  = obj
      CHANGING
        c_package_data             = _devc_data
      EXCEPTIONS
        object_already_existing    = 1
        object_just_created        = 2
        not_authorized             = 3
        wrong_name_prefix          = 4
        undefined_name             = 5
        reserved_local_name        = 6
        invalid_package_name       = 7
        short_text_missing         = 8
        software_component_invalid = 9
        layer_invalid              = 10
        author_not_existing        = 11
        component_not_existing     = 12
        component_missing          = 13
        prefix_in_use              = 14
        unexpected_error           = 15
        intern_err                 = 16
        OTHERS                     = 17
            .
  ENDIF.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'All ready exist'.
      WHEN 2 OR 3.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>not_authorized.
      WHEN 4 OR 5 OR 6 OR 7.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'Problem with Dev Class name'.
      WHEN 8.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'Problem with Short Text'.
      WHEN 9.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'Problem with SoftwareComponent'.
      WHEN 10.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'Problem with Layer'.
      WHEN 12 OR 13.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>error_message
            msg = 'Problem with Component'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_saplink
          EXPORTING
            textid = zcx_saplink=>system_error.
    ENDCASE.
  ENDIF.

  CALL METHOD obj->save
*    EXPORTING
*      i_transport_request   =
*    IMPORTING
*      e_transport_request   =
    EXCEPTIONS
      object_invalid        = 1
      object_not_changeable = 2
      cancelled_in_corr     = 3
      permission_failure    = 4
      unexpected_error      = 5
      intern_err            = 6
      OTHERS                = 7
          .
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_saplink
      EXPORTING
        textid = zcx_saplink=>error_message
        msg = 'Dev Class creation failed !'.
  ENDIF.


ENDMETHOD.


METHOD deleteobject.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/

  load_obj( ).
  CALL METHOD obj->delete
    EXCEPTIONS
      object_not_empty      = 1
      object_not_changeable = 2
      object_invalid        = 3
      intern_err            = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CALL METHOD obj->save
    EXPORTING
      i_transport_request   = space
*   IMPORTING
*     e_transport_request   =
    EXCEPTIONS
      object_invalid        = 1
      object_not_changeable = 2
      cancelled_in_corr     = 3
      permission_failure    = 4
      unexpected_error      = 5
      intern_err            = 6
      OTHERS                = 7
          .
  IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDMETHOD.


method GETOBJECTTYPE.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/

  objecttype = 'DEVC'.  "Dev class

endmethod.


method LOAD_OBJ.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/
  data _key type DEVCLASS.

  _key = objname.
  CALL METHOD cl_package=>if_package~load_package
    EXPORTING
      i_package_name             = _key
      i_force_reload             = 'X'
    IMPORTING
      e_package                  = obj
    EXCEPTIONS
      object_not_existing        = 1
      unexpected_error           = 2
      intern_err                 = 3
      object_locked_and_modified = 4
      others                     = 5
          .
  IF sy-subrc <> 0.
    clear obj.
  ENDIF.
endmethod.


METHOD obj_2_struct.
*/---------------------------------------------------------------------\
*|   This plugin have been don by : Taryck BENSIALI                    |
*|                                                                     |
*|   e-mail : taryck@bensiali.net                                      |
*|                                                                     |
*|   this plugin is distributed in the hope that it will be useful,    |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/
  DATA:
    of TYPE string,
    desc TYPE REF TO cl_abap_typedescr,
    fields TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS:
    <map> LIKE LINE OF mapping,
    <field> LIKE LINE OF fields->components,
    <sf> TYPE ANY,
    <of> TYPE ANY.

  CALL METHOD cl_abap_datadescr=>describe_by_data
    EXPORTING
      p_data      = struct
    RECEIVING
      p_descr_ref = desc.

  CHECK desc->kind = cl_abap_typedescr=>kind_struct.
  CHECK desc->type_kind = cl_abap_typedescr=>typekind_struct1
     OR desc->type_kind = cl_abap_typedescr=>typekind_struct2.

  fields ?= desc.
  CHECK fields->struct_kind = cl_abap_structdescr=>structkind_flat.

  LOOP AT fields->components ASSIGNING <field>.
* Check for exception
    READ TABLE mapping WITH TABLE KEY st_fn = <field>-name
                       ASSIGNING <map>.
    IF sy-subrc <> 0.
      CONCATENATE 'OBJ->' <field>-name INTO of.
    ELSE.
      CONCATENATE 'OBJ->' <map>-obj_fn INTO of.
    ENDIF.
    ASSIGN (of) TO <of>.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT <field>-name OF STRUCTURE struct TO <sf>.
    CHECK sy-subrc = 0.

    TRY.
        <sf> = <of>.
      CATCH cx_root.
    ENDTRY.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
