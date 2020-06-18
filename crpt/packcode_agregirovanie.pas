{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  base on docs from http://api-docs.diadoc.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit packcode_agregirovanie;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject;

type
  //Идентификационный номер налогоплательщика - физического лица
  TSP_TIN_type = String;
  //Идентификационный номер налогоплательщика - юридического лица
  TLP_TIN_type = String;
  //Код причины постановки на учет (КПП) - 5 и 6 знаки от 0-9 и A-Z
  TRRC_type = String;
  //Код из Справочника субъекта Российской Федерации constituent entity of the Russian Federation registry code
  TCERFR_code = String;
  //Код из Общероссийского классификатора стран мира RNCC (Russian National Classification of Countries) code
  TRNCC_code = String;
  //Произвольный текст длиной от 1 до 36 символов
  Tstring_36 = String;
  //Тип "Дата со временем" (с временной зоной)
  Tdatetimeoffset = TDateTime;
  //Строка произвольной длины, но не более 200 символов
  Tstring200_type = string;
  //Строка произвольной длины, но не более 500 символов
  Tstring500_type = string;
  //Строка произвольной длины, но не более 1000 символов
  Tstring1000_type = string;
  //Должность  физического лица
  Tperson_position = String;
  //Тип "Код ТН ВЭД ЕАЭС"
  Ttnved4_type = String;
  //Индивидуальный серийный номер потребительской упаковки
  Tcis_type = String;
  //Индивидуальный серийный номер групповой/транспортной упаковки
  Tsscc_type = String;
  //Номер кода агрегата
  Tpack_code_type = String;
  //Тип "Строка" (не пустая в начале)
  Tstring_clear_at_begin_type = String;
  //Тип "Строка" (без спецсимволов с обеих сторон)
  Tstring_clear_type = string;
  //Порядковый номер документа
  Tdocument_number = String;
  //Дата в формате ГГГГ-ММ-ДД (2019-04-12 - 2019-04-12)
  Tdate_type = String;
  //Время в формате ЧЧ.ММ.СС
  Ttime_type = String;
type

  {  Forward declarations  }
  Torganisation_type = class;
  Torganisation_type_id_info = class;
  Torganisation_type_id_info_LP_info = class;
  Torganisation_type_id_info_foreign_entity = class;
  Torganisation_type_id_info_foreign_SP = class;
  TSP_info_type = class;
  Tfull_name_type = class;
  TAddress_type = class;
  Tlocation_address_type = class;
  TRU_address_type = class;
  Tcontacts_type = class;
  Tofficer_type = class;
  Tunit_pack = class;
  Tunit_pack_Document = class;
  Tunit_pack_Document_pack_content = class;

  {  Generic classes for collections  }
  Torganisation_typeList = specialize GXMLSerializationObjectList<Torganisation_type>;
  Torganisation_type_id_infoList = specialize GXMLSerializationObjectList<Torganisation_type_id_info>;
  Torganisation_type_id_info_LP_infoList = specialize GXMLSerializationObjectList<Torganisation_type_id_info_LP_info>;
  Torganisation_type_id_info_foreign_entityList = specialize GXMLSerializationObjectList<Torganisation_type_id_info_foreign_entity>;
  Torganisation_type_id_info_foreign_SPList = specialize GXMLSerializationObjectList<Torganisation_type_id_info_foreign_SP>;
  TSP_info_typeList = specialize GXMLSerializationObjectList<TSP_info_type>;
  Tfull_name_typeList = specialize GXMLSerializationObjectList<Tfull_name_type>;
  TAddress_typeList = specialize GXMLSerializationObjectList<TAddress_type>;
  Tlocation_address_typeList = specialize GXMLSerializationObjectList<Tlocation_address_type>;
  TRU_address_typeList = specialize GXMLSerializationObjectList<TRU_address_type>;
  Tcontacts_typeList = specialize GXMLSerializationObjectList<Tcontacts_type>;
  Tofficer_typeList = specialize GXMLSerializationObjectList<Tofficer_type>;
  Tunit_packList = specialize GXMLSerializationObjectList<Tunit_pack>;
  Tunit_pack_DocumentList = specialize GXMLSerializationObjectList<Tunit_pack_Document>;
  Tunit_pack_Document_pack_contentList = specialize GXMLSerializationObjectList<Tunit_pack_Document_pack_content>;

  {  Torganisation_type  }
  //Сведения об участнике факта хозяйственной жизни
  Torganisation_type = class(TXmlSerializationObject)
  private
    Fid_info:Torganisation_type_id_info;
    FAddress:TAddress_type;
    Fcontacts:Tcontacts_type;
    FOKPO_code:String;
    Fstructural_unit:String;
    procedure SetOKPO_code( AValue:String);
    procedure Setstructural_unit( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Идентификационные сведения
    property id_info:Torganisation_type_id_info read Fid_info;
    //Адрес
    property Address:TAddress_type read FAddress;
    //Контактные сведения
    property contacts:Tcontacts_type read Fcontacts;
    //Код в общероссийском классификаторе предприятий и организаций
    property OKPO_code:String read FOKPO_code write SetOKPO_code;
    //Структурное подразделение
    property structural_unit:String read Fstructural_unit write Setstructural_unit;
  end;

  {  Torganisation_type_id_info  }
  Torganisation_type_id_info = class(TXmlSerializationObject)
  private
    FSP_info:TSP_info_type;
    FLP_info:Torganisation_type_id_info_LP_info;
    Fforeign_entity:Torganisation_type_id_info_foreign_entity;
    Fforeign_SP:Torganisation_type_id_info_foreign_SP;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Сведения об индивидуальном предпринимателеsole proprietor information
    property SP_info:TSP_info_type read FSP_info;
    //Сведения о юридическом лице, состоящем на учете в налоговых органахlegal person information
    property LP_info:Torganisation_type_id_info_LP_info read FLP_info;
    //Сведения об иностранном лице, не состоящем на учете в налоговых органах
    property foreign_entity:Torganisation_type_id_info_foreign_entity read Fforeign_entity;
    //Сведения об иностранном лице, не состоящем на учете в налоговых органах
    property foreign_SP:Torganisation_type_id_info_foreign_SP read Fforeign_SP;
  end;

  {  Torganisation_type_id_info_LP_info  }
  Torganisation_type_id_info_LP_info = class(TXmlSerializationObject)
  private
    Forg_name:String;
    FLP_TIN:TLP_TIN_type;
    FRRC:TRRC_type;
    procedure Setorg_name( AValue:String);
    procedure SetLP_TIN( AValue:TLP_TIN_type);
    procedure SetRRC( AValue:TRRC_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Наименование полное
    property org_name:String read Forg_name write Setorg_name;
    //ИННtaxpayer ID number
    property LP_TIN:TLP_TIN_type read FLP_TIN write SetLP_TIN;
    //КППtax registration reason code
    property RRC:TRRC_type read FRRC write SetRRC;
  end;

  {  Torganisation_type_id_info_foreign_entity  }
  Torganisation_type_id_info_foreign_entity = class(TXmlSerializationObject)
  private
    Forg_name:String;
    Fother_info:String;
    procedure Setorg_name( AValue:String);
    procedure Setother_info( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Наименование полное
    property org_name:String read Forg_name write Setorg_name;
    //Иные сведения, идентифицирующие юридическое лицоВ частности, может быть указана страна при отсутствии country_code
    property other_info:String read Fother_info write Setother_info;
  end;

  {  Torganisation_type_id_info_foreign_SP  }
  Torganisation_type_id_info_foreign_SP = class(TXmlSerializationObject)
  private
    Forg_name:String;
    Fother_info:String;
    procedure Setorg_name( AValue:String);
    procedure Setother_info( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Наименование полное
    property org_name:String read Forg_name write Setorg_name;
    //Иные сведения, идентифицирующие юридическое лицоВ частности, может быть указана страна при отсутствии country_code
    property other_info:String read Fother_info write Setother_info;
  end;

  {  TSP_info_type  }
  //Сведения об индивидуальном предпринимателеsole proprietor information
  TSP_info_type = class(TXmlSerializationObject)
  private
    Ffull_name:Tfull_name_type;
    FSP_TIN:TSP_TIN_type;
    Freg_sertificate:String;
    Fother_info:String;
    procedure SetSP_TIN( AValue:TSP_TIN_type);
    procedure Setreg_sertificate( AValue:String);
    procedure Setother_info( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Фамилия, Имя, Отчество
    property full_name:Tfull_name_type read Ffull_name;
    //ИНН физического лицаsole proprietor taxpayer identification number
    property SP_TIN:TSP_TIN_type read FSP_TIN write SetSP_TIN;
    //Реквизиты свидетельства о государственной регистрации индивидуального предпринимателяОбязателен для случаев подписания счета-фактуры непосредственно продавцом
    property reg_sertificate:String read Freg_sertificate write Setreg_sertificate;
    //Иные сведения, идентифицирующие физическое лицо
    property other_info:String read Fother_info write Setother_info;
  end;

  {  Tfull_name_type  }
  //Фамилия, имя, отчество физического лица
  Tfull_name_type = class(TXmlSerializationObject)
  private
    Fsurname:String;
    Ffirst_name:String;
    Fpatronymic:String;
    procedure Setsurname( AValue:String);
    procedure Setfirst_name( AValue:String);
    procedure Setpatronymic( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Фамилия
    property surname:String read Fsurname write Setsurname;
    //Имя
    property first_name:String read Ffirst_name write Setfirst_name;
    //Отчество
    property patronymic:String read Fpatronymic write Setpatronymic;
  end;

  {  TAddress_type  }
  //Сведения об адресе
  TAddress_type = class(TXmlSerializationObject)
  private
    FRU_address:TRU_address_type;
    Flocation_address:Tlocation_address_type;
    FSAR_code:Tstring_36;
    procedure SetSAR_code( AValue:Tstring_36);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Адрес местонахождения/почтовый адрес (реквизиты адреса на территории Российской Федерации)
    property RU_address:TRU_address_type read FRU_address;
    //Адрес местонахождения/почтовый адрес (информация об адресе, в том числе об адресе за пределами территории Российской Федерации)
    property location_address:Tlocation_address_type read Flocation_address;
    //Уникальный номер адреса объекта адресации в государственном адресном реестреaddressing object unique code in State Address Registry
    property SAR_code:Tstring_36 read FSAR_code write SetSAR_code;
  end;

  {  Tlocation_address_type  }
  //Информация об адресе, в том числе об адресе за пределами территории Российской Федерации
  Tlocation_address_type = class(TXmlSerializationObject)
  private
    Fcountry_code:TRNCC_code;
    Ftext_address:String;
    procedure Setcountry_code( AValue:TRNCC_code);
    procedure Settext_address( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Код страны
    property country_code:TRNCC_code read Fcountry_code write Setcountry_code;
    //Адрес
    property text_address:String read Ftext_address write Settext_address;
  end;

  {  TRU_address_type  }
  //Адрес в Российской Федерации
  TRU_address_type = class(TXmlSerializationObject)
  private
    Fzip_code:String;
    Fregion_code:TCERFR_code;
    Fdistrict_name:String;
    Fcity_name:String;
    Flocality_name:String;
    Fstreet_name:String;
    Fhouse_number:String;
    Fbulk_number:String;
    Fflat_number:String;
    procedure Setzip_code( AValue:String);
    procedure Setregion_code( AValue:TCERFR_code);
    procedure Setdistrict_name( AValue:String);
    procedure Setcity_name( AValue:String);
    procedure Setlocality_name( AValue:String);
    procedure Setstreet_name( AValue:String);
    procedure Sethouse_number( AValue:String);
    procedure Setbulk_number( AValue:String);
    procedure Setflat_number( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Индекс
    property zip_code:String read Fzip_code write Setzip_code;
    //Код региона
    property region_code:TCERFR_code read Fregion_code write Setregion_code;
    //Район
    property district_name:String read Fdistrict_name write Setdistrict_name;
    //Город
    property city_name:String read Fcity_name write Setcity_name;
    //Населенный пункт
    property locality_name:String read Flocality_name write Setlocality_name;
    //Улица
    property street_name:String read Fstreet_name write Setstreet_name;
    //Дом
    property house_number:String read Fhouse_number write Sethouse_number;
    //Корпус
    property bulk_number:String read Fbulk_number write Setbulk_number;
    //Квартира
    property flat_number:String read Fflat_number write Setflat_number;
  end;

  {  Tcontacts_type  }
  //Контактные данные
  Tcontacts_type = class(TXmlSerializationObject)
  private
    Fphone_number:String;
    Femail:String;
    procedure Setphone_number( AValue:String);
    procedure Setemail( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Номер контактного телефона/факс
    property phone_number:String read Fphone_number write Setphone_number;
    //Адрес электронной почты
    property email:String read Femail write Setemail;
  end;

  {  Tofficer_type  }
  //Сведения о должностном лице
  Tofficer_type = class(TXmlSerializationObject)
  private
    Ffull_name:Tfull_name_type;
    Fofficer_position:Tperson_position;
    procedure Setofficer_position( AValue:Tperson_position);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Фамилия, Имя, Отчество
    property full_name:Tfull_name_type read Ffull_name;
    //Должность
    property officer_position:Tperson_position read Fofficer_position write Setofficer_position;
  end;

  {  Tunit_pack  }
  Tunit_pack = class(TXmlSerializationObject)
  private
    FDocument:Tunit_pack_Document;
    Fdocument_id:String;
    FVerForm:String;
    Ffile_date_time:Tdatetimeoffset;
    FVerProg:String;
    Faction_id:Longint;
    Fversion:String;
    procedure Setdocument_id( AValue:String);
    procedure SetVerForm( AValue:String);
    procedure Setfile_date_time( AValue:Tdatetimeoffset);
    procedure SetVerProg( AValue:String);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Document:Tunit_pack_Document read FDocument;
    //Идентификатор файла
    property document_id:String read Fdocument_id write Setdocument_id;
    //Версия формата
    property VerForm:String read FVerForm write SetVerForm;
    //Дата и время формирования файлаДата и время с учетом часового пояса в формате ГГГГ-ММ-ДДTЧЧ:ММ:СС+(-)ЧЧ:ММ
    property file_date_time:Tdatetimeoffset read Ffile_date_time write Setfile_date_time;
    //Наименование и версия программы, с помощью которой сформирован файл
    property VerProg:String read FVerProg write SetVerProg;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:String read Fversion write Setversion;
  end;

  {  Tunit_pack_Document  }
  Tunit_pack_Document = class(TXmlSerializationObject)
  private
    Forganisation:Torganisation_type;
    Fpack_content:Tunit_pack_Document_pack_contentList;
    Foperation_date_time:Tdatetimeoffset;
    Fdocument_number:String;
    procedure Setoperation_date_time( AValue:Tdatetimeoffset);
    procedure Setdocument_number( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Сведения об организации / ИП
    property organisation:Torganisation_type read Forganisation;
    //Содержание упаковки
    property pack_content:Tunit_pack_Document_pack_contentList read Fpack_content;
    //Дата и время совершения операции агрегированияДата и время с учетом часового пояса в формате ГГГГ-ММ-ДДTЧЧ:ММ:СС+(-)ЧЧ:ММ
    property operation_date_time:Tdatetimeoffset read Foperation_date_time write Setoperation_date_time;
    //Номер документа во внутренней системе учета Участника
    property document_number:String read Fdocument_number write Setdocument_number;
  end;

  {  Tunit_pack_Document_pack_content  }
  Tunit_pack_Document_pack_content = class(TXmlSerializationObject)
  private
    Fpack_code:Tpack_code_type;
    Fcis:Tcis_type;
    Fsscc:Tsscc_type;
    procedure Setpack_code( AValue:Tpack_code_type);
    procedure Setcis( AValue:Tcis_type);
    procedure Setsscc( AValue:Tsscc_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Номер кода агрегата (упаковки)
    property pack_code:Tpack_code_type read Fpack_code write Setpack_code;
    property cis:Tcis_type read Fcis write Setcis;
    property sscc:Tsscc_type read Fsscc write Setsscc;
  end;

implementation

  {  Torganisation_type  }
procedure Torganisation_type.SetOKPO_code(AValue: String);
begin
  CheckStrMinSize('OKPO_code', AValue);
  CheckStrMaxSize('OKPO_code', AValue);
  FOKPO_code:=AValue;
  ModifiedProperty('OKPO_code');
end;

procedure Torganisation_type.Setstructural_unit(AValue: String);
begin
  CheckStrMinSize('structural_unit', AValue);
  CheckStrMaxSize('structural_unit', AValue);
  Fstructural_unit:=AValue;
  ModifiedProperty('structural_unit');
end;

procedure Torganisation_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('id_info', 'id_info', [], '', -1, -1);
  P:=RegisterProperty('Address', 'Address', [], '', -1, -1);
  P:=RegisterProperty('contacts', 'contacts', [], '', -1, -1);
  P:=RegisterProperty('OKPO_code', 'OKPO_code', [], '', 1, 10);
  P:=RegisterProperty('structural_unit', 'structural_unit', [], '', 1, 1000);
end;

procedure Torganisation_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fid_info:=Torganisation_type_id_info.Create;
  FAddress:=TAddress_type.Create;
  Fcontacts:=Tcontacts_type.Create;
end;

destructor Torganisation_type.Destroy;
begin
  Fid_info.Free;
  FAddress.Free;
  Fcontacts.Free;
  inherited Destroy;
end;

constructor Torganisation_type.Create;
begin
  inherited Create;
end;

  {  Torganisation_type_id_info  }
procedure Torganisation_type_id_info.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('SP_info', 'SP_info', [], '', -1, -1);
  P:=RegisterProperty('LP_info', 'LP_info', [], '', -1, -1);
  P:=RegisterProperty('foreign_entity', 'foreign_entity', [], '', -1, -1);
  P:=RegisterProperty('foreign_SP', 'foreign_SP', [], '', -1, -1);
end;

procedure Torganisation_type_id_info.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSP_info:=TSP_info_type.Create;
  FLP_info:=Torganisation_type_id_info_LP_info.Create;
  Fforeign_entity:=Torganisation_type_id_info_foreign_entity.Create;
  Fforeign_SP:=Torganisation_type_id_info_foreign_SP.Create;
end;

destructor Torganisation_type_id_info.Destroy;
begin
  FSP_info.Free;
  FLP_info.Free;
  Fforeign_entity.Free;
  Fforeign_SP.Free;
  inherited Destroy;
end;

constructor Torganisation_type_id_info.Create;
begin
  inherited Create;
end;

  {  Torganisation_type_id_info_LP_info  }
procedure Torganisation_type_id_info_LP_info.Setorg_name(AValue: String);
begin
  CheckStrMinSize('org_name', AValue);
  CheckStrMaxSize('org_name', AValue);
  Forg_name:=AValue;
  ModifiedProperty('org_name');
end;

procedure Torganisation_type_id_info_LP_info.SetLP_TIN(AValue: TLP_TIN_type);
begin
  CheckStrMinSize('LP_TIN', AValue);
  CheckStrMaxSize('LP_TIN', AValue);
  FLP_TIN:=AValue;
  ModifiedProperty('LP_TIN');
end;

procedure Torganisation_type_id_info_LP_info.SetRRC(AValue: TRRC_type);
begin
  CheckStrMinSize('RRC', AValue);
  CheckStrMaxSize('RRC', AValue);
  FRRC:=AValue;
  ModifiedProperty('RRC');
end;

procedure Torganisation_type_id_info_LP_info.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('org_name', 'org_name', [xsaRequared], '', 1, 1000);
  P:=RegisterProperty('LP_TIN', 'LP_TIN', [xsaRequared], '', 10, 10);
  P:=RegisterProperty('RRC', 'RRC', [], '', 9, 9);
end;

procedure Torganisation_type_id_info_LP_info.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Torganisation_type_id_info_LP_info.Destroy;
begin
  inherited Destroy;
end;

constructor Torganisation_type_id_info_LP_info.Create;
begin
  inherited Create;
end;

  {  Torganisation_type_id_info_foreign_entity  }
procedure Torganisation_type_id_info_foreign_entity.Setorg_name(AValue: String);
begin
  CheckStrMinSize('org_name', AValue);
  CheckStrMaxSize('org_name', AValue);
  Forg_name:=AValue;
  ModifiedProperty('org_name');
end;

procedure Torganisation_type_id_info_foreign_entity.Setother_info(AValue: String);
begin
  CheckStrMinSize('other_info', AValue);
  CheckStrMaxSize('other_info', AValue);
  Fother_info:=AValue;
  ModifiedProperty('other_info');
end;

procedure Torganisation_type_id_info_foreign_entity.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('org_name', 'org_name', [xsaRequared], '', 1, 1000);
  P:=RegisterProperty('other_info', 'other_info', [], '', 1, 255);
end;

procedure Torganisation_type_id_info_foreign_entity.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Torganisation_type_id_info_foreign_entity.Destroy;
begin
  inherited Destroy;
end;

constructor Torganisation_type_id_info_foreign_entity.Create;
begin
  inherited Create;
end;

  {  Torganisation_type_id_info_foreign_SP  }
procedure Torganisation_type_id_info_foreign_SP.Setorg_name(AValue: String);
begin
  CheckStrMinSize('org_name', AValue);
  CheckStrMaxSize('org_name', AValue);
  Forg_name:=AValue;
  ModifiedProperty('org_name');
end;

procedure Torganisation_type_id_info_foreign_SP.Setother_info(AValue: String);
begin
  CheckStrMinSize('other_info', AValue);
  CheckStrMaxSize('other_info', AValue);
  Fother_info:=AValue;
  ModifiedProperty('other_info');
end;

procedure Torganisation_type_id_info_foreign_SP.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('org_name', 'org_name', [xsaRequared], '', 1, 1000);
  P:=RegisterProperty('other_info', 'other_info', [], '', 1, 255);
end;

procedure Torganisation_type_id_info_foreign_SP.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Torganisation_type_id_info_foreign_SP.Destroy;
begin
  inherited Destroy;
end;

constructor Torganisation_type_id_info_foreign_SP.Create;
begin
  inherited Create;
end;

  {  TSP_info_type  }
procedure TSP_info_type.SetSP_TIN(AValue: TSP_TIN_type);
begin
  CheckStrMinSize('SP_TIN', AValue);
  CheckStrMaxSize('SP_TIN', AValue);
  FSP_TIN:=AValue;
  ModifiedProperty('SP_TIN');
end;

procedure TSP_info_type.Setreg_sertificate(AValue: String);
begin
  CheckStrMinSize('reg_sertificate', AValue);
  CheckStrMaxSize('reg_sertificate', AValue);
  Freg_sertificate:=AValue;
  ModifiedProperty('reg_sertificate');
end;

procedure TSP_info_type.Setother_info(AValue: String);
begin
  CheckStrMinSize('other_info', AValue);
  CheckStrMaxSize('other_info', AValue);
  Fother_info:=AValue;
  ModifiedProperty('other_info');
end;

procedure TSP_info_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('full_name', 'full_name', [], '', -1, -1);
  P:=RegisterProperty('SP_TIN', 'SP_TIN', [xsaRequared], '', 12, 12);
  P:=RegisterProperty('reg_sertificate', 'reg_sertificate', [], '', 1, 100);
  P:=RegisterProperty('other_info', 'other_info', [], '', 1, 255);
end;

procedure TSP_info_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Ffull_name:=Tfull_name_type.Create;
end;

destructor TSP_info_type.Destroy;
begin
  Ffull_name.Free;
  inherited Destroy;
end;

constructor TSP_info_type.Create;
begin
  inherited Create;
end;

  {  Tfull_name_type  }
procedure Tfull_name_type.Setsurname(AValue: String);
begin
  CheckStrMinSize('surname', AValue);
  CheckStrMaxSize('surname', AValue);
  Fsurname:=AValue;
  ModifiedProperty('surname');
end;

procedure Tfull_name_type.Setfirst_name(AValue: String);
begin
  CheckStrMinSize('first_name', AValue);
  CheckStrMaxSize('first_name', AValue);
  Ffirst_name:=AValue;
  ModifiedProperty('first_name');
end;

procedure Tfull_name_type.Setpatronymic(AValue: String);
begin
  CheckStrMinSize('patronymic', AValue);
  CheckStrMaxSize('patronymic', AValue);
  Fpatronymic:=AValue;
  ModifiedProperty('patronymic');
end;

procedure Tfull_name_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('surname', 'surname', [xsaRequared], '', 1, 60);
  P:=RegisterProperty('first_name', 'first_name', [xsaRequared], '', 1, 60);
  P:=RegisterProperty('patronymic', 'patronymic', [], '', 1, 60);
end;

procedure Tfull_name_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tfull_name_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tfull_name_type.Create;
begin
  inherited Create;
end;

  {  TAddress_type  }
procedure TAddress_type.SetSAR_code(AValue: Tstring_36);
begin
  CheckStrMinSize('SAR_code', AValue);
  CheckStrMaxSize('SAR_code', AValue);
  FSAR_code:=AValue;
  ModifiedProperty('SAR_code');
end;

procedure TAddress_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('RU_address', 'RU_address', [], '', -1, -1);
  P:=RegisterProperty('location_address', 'location_address', [], '', -1, -1);
  P:=RegisterProperty('SAR_code', 'SAR_code', [xsaSimpleObject], '', 1, 36);
end;

procedure TAddress_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FRU_address:=TRU_address_type.Create;
  Flocation_address:=Tlocation_address_type.Create;
end;

destructor TAddress_type.Destroy;
begin
  FRU_address.Free;
  Flocation_address.Free;
  inherited Destroy;
end;

constructor TAddress_type.Create;
begin
  inherited Create;
end;

  {  Tlocation_address_type  }
procedure Tlocation_address_type.Setcountry_code(AValue: TRNCC_code);
begin
  CheckStrMinSize('country_code', AValue);
  CheckStrMaxSize('country_code', AValue);
  Fcountry_code:=AValue;
  ModifiedProperty('country_code');
end;

procedure Tlocation_address_type.Settext_address(AValue: String);
begin
  CheckStrMinSize('text_address', AValue);
  CheckStrMaxSize('text_address', AValue);
  Ftext_address:=AValue;
  ModifiedProperty('text_address');
end;

procedure Tlocation_address_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('country_code', 'country_code', [xsaRequared], '', 3, 3);
  P:=RegisterProperty('text_address', 'text_address', [xsaRequared], '', 1, 255);
end;

procedure Tlocation_address_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tlocation_address_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tlocation_address_type.Create;
begin
  inherited Create;
end;

  {  TRU_address_type  }
procedure TRU_address_type.Setzip_code(AValue: String);
begin
  CheckStrMinSize('zip_code', AValue);
  CheckStrMaxSize('zip_code', AValue);
  Fzip_code:=AValue;
  ModifiedProperty('zip_code');
end;

procedure TRU_address_type.Setregion_code(AValue: TCERFR_code);
begin
  CheckStrMinSize('region_code', AValue);
  CheckStrMaxSize('region_code', AValue);
  Fregion_code:=AValue;
  ModifiedProperty('region_code');
end;

procedure TRU_address_type.Setdistrict_name(AValue: String);
begin
  CheckStrMinSize('district_name', AValue);
  CheckStrMaxSize('district_name', AValue);
  Fdistrict_name:=AValue;
  ModifiedProperty('district_name');
end;

procedure TRU_address_type.Setcity_name(AValue: String);
begin
  CheckStrMinSize('city_name', AValue);
  CheckStrMaxSize('city_name', AValue);
  Fcity_name:=AValue;
  ModifiedProperty('city_name');
end;

procedure TRU_address_type.Setlocality_name(AValue: String);
begin
  CheckStrMinSize('locality_name', AValue);
  CheckStrMaxSize('locality_name', AValue);
  Flocality_name:=AValue;
  ModifiedProperty('locality_name');
end;

procedure TRU_address_type.Setstreet_name(AValue: String);
begin
  CheckStrMinSize('street_name', AValue);
  CheckStrMaxSize('street_name', AValue);
  Fstreet_name:=AValue;
  ModifiedProperty('street_name');
end;

procedure TRU_address_type.Sethouse_number(AValue: String);
begin
  CheckStrMinSize('house_number', AValue);
  CheckStrMaxSize('house_number', AValue);
  Fhouse_number:=AValue;
  ModifiedProperty('house_number');
end;

procedure TRU_address_type.Setbulk_number(AValue: String);
begin
  CheckStrMinSize('bulk_number', AValue);
  CheckStrMaxSize('bulk_number', AValue);
  Fbulk_number:=AValue;
  ModifiedProperty('bulk_number');
end;

procedure TRU_address_type.Setflat_number(AValue: String);
begin
  CheckStrMinSize('flat_number', AValue);
  CheckStrMaxSize('flat_number', AValue);
  Fflat_number:=AValue;
  ModifiedProperty('flat_number');
end;

procedure TRU_address_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('zip_code', 'zip_code', [], '', 6, 6);
  P:=RegisterProperty('region_code', 'region_code', [xsaRequared], '', 2, 2);
  P:=RegisterProperty('district_name', 'district_name', [], '', 1, 50);
  P:=RegisterProperty('city_name', 'city_name', [], '', 1, 50);
  P:=RegisterProperty('locality_name', 'locality_name', [], '', 1, 50);
  P:=RegisterProperty('street_name', 'street_name', [], '', 1, 50);
  P:=RegisterProperty('house_number', 'house_number', [], '', 1, 20);
  P:=RegisterProperty('bulk_number', 'bulk_number', [], '', 1, 20);
  P:=RegisterProperty('flat_number', 'flat_number', [], '', 1, 20);
end;

procedure TRU_address_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TRU_address_type.Destroy;
begin
  inherited Destroy;
end;

constructor TRU_address_type.Create;
begin
  inherited Create;
end;

  {  Tcontacts_type  }
procedure Tcontacts_type.Setphone_number(AValue: String);
begin
  CheckStrMinSize('phone_number', AValue);
  CheckStrMaxSize('phone_number', AValue);
  Fphone_number:=AValue;
  ModifiedProperty('phone_number');
end;

procedure Tcontacts_type.Setemail(AValue: String);
begin
  CheckStrMinSize('email', AValue);
  CheckStrMaxSize('email', AValue);
  Femail:=AValue;
  ModifiedProperty('email');
end;

procedure Tcontacts_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('phone_number', 'phone_number', [], '', 1, 255);
  P:=RegisterProperty('email', 'email', [], '', 1, 255);
end;

procedure Tcontacts_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tcontacts_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tcontacts_type.Create;
begin
  inherited Create;
end;

  {  Tofficer_type  }
procedure Tofficer_type.Setofficer_position(AValue: Tperson_position);
begin
  CheckStrMinSize('officer_position', AValue);
  CheckStrMaxSize('officer_position', AValue);
  Fofficer_position:=AValue;
  ModifiedProperty('officer_position');
end;

procedure Tofficer_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('full_name', 'full_name', [], '', -1, -1);
  P:=RegisterProperty('officer_position', 'officer_position', [], '', 1, 200);
end;

procedure Tofficer_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Ffull_name:=Tfull_name_type.Create;
end;

destructor Tofficer_type.Destroy;
begin
  Ffull_name.Free;
  inherited Destroy;
end;

constructor Tofficer_type.Create;
begin
  inherited Create;
end;

  {  Tunit_pack  }
procedure Tunit_pack.Setdocument_id(AValue: String);
begin
  CheckStrMinSize('document_id', AValue);
  CheckStrMaxSize('document_id', AValue);
  Fdocument_id:=AValue;
  ModifiedProperty('document_id');
end;

procedure Tunit_pack.SetVerForm(AValue: String);
begin
  CheckLockupValue('VerForm', AValue);
  CheckStrMinSize('VerForm', AValue);
  CheckStrMaxSize('VerForm', AValue);
  FVerForm:=AValue;
  ModifiedProperty('VerForm');
end;

procedure Tunit_pack.Setfile_date_time(AValue: Tdatetimeoffset);
begin
  Ffile_date_time:=AValue;
  ModifiedProperty('file_date_time');
end;

procedure Tunit_pack.SetVerProg(AValue: String);
begin
  CheckStrMinSize('VerProg', AValue);
  CheckStrMaxSize('VerProg', AValue);
  FVerProg:=AValue;
  ModifiedProperty('VerProg');
end;

procedure Tunit_pack.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tunit_pack.Setversion(AValue: String);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tunit_pack.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('Document', 'Document', [], '', -1, -1);
  P:=RegisterProperty('document_id', 'document_id', [xsaRequared], '', 1, 150);
  P:=RegisterProperty('VerForm', 'VerForm', [xsaRequared], '', 1, 5);
    P.ValidList.Add('1.03');
  P:=RegisterProperty('file_date_time', 'file_date_time', [xsaRequared], '', -1, -1);
  P:=RegisterProperty('VerProg', 'VerProg', [], '', 1, 40);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='30';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='1';
end;

procedure Tunit_pack.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FDocument:=Tunit_pack_Document.Create;
end;

destructor Tunit_pack.Destroy;
begin
  FDocument.Free;
  inherited Destroy;
end;

function Tunit_pack.RootNodeName:string;
begin
  Result:='unit_pack';
end;

constructor Tunit_pack.Create;
begin
  inherited Create;
  action_id:=30;
  version:='1';
end;

  {  Tunit_pack_Document  }
procedure Tunit_pack_Document.Setoperation_date_time(AValue: Tdatetimeoffset);
begin
  Foperation_date_time:=AValue;
  ModifiedProperty('operation_date_time');
end;

procedure Tunit_pack_Document.Setdocument_number(AValue: String);
begin
  CheckStrMinSize('document_number', AValue);
  CheckStrMaxSize('document_number', AValue);
  Fdocument_number:=AValue;
  ModifiedProperty('document_number');
end;

procedure Tunit_pack_Document.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('organisation', 'organisation', [], '', -1, -1);
  P:=RegisterProperty('pack_content', 'pack_content', [], '', -1, -1);
  P:=RegisterProperty('operation_date_time', 'operation_date_time', [xsaRequared], '', -1, -1);
  P:=RegisterProperty('document_number', 'document_number', [xsaRequared], '', 1, 150);
end;

procedure Tunit_pack_Document.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Forganisation:=Torganisation_type.Create;
  Fpack_content:=Tunit_pack_Document_pack_contentList.Create;
end;

destructor Tunit_pack_Document.Destroy;
begin
  Forganisation.Free;
  Fpack_content.Free;
  inherited Destroy;
end;

constructor Tunit_pack_Document.Create;
begin
  inherited Create;
end;

  {  Tunit_pack_Document_pack_content  }
procedure Tunit_pack_Document_pack_content.Setpack_code(AValue: Tpack_code_type);
begin
  CheckStrMinSize('pack_code', AValue);
  CheckStrMaxSize('pack_code', AValue);
  Fpack_code:=AValue;
  ModifiedProperty('pack_code');
end;

procedure Tunit_pack_Document_pack_content.Setcis(AValue: Tcis_type);
begin
  CheckStrMinSize('cis', AValue);
  CheckStrMaxSize('cis', AValue);
  Fcis:=AValue;
  ModifiedProperty('cis');
end;

procedure Tunit_pack_Document_pack_content.Setsscc(AValue: Tsscc_type);
begin
  CheckStrMinSize('sscc', AValue);
  CheckStrMaxSize('sscc', AValue);
  Fsscc:=AValue;
  ModifiedProperty('sscc');
end;

procedure Tunit_pack_Document_pack_content.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('pack_code', 'pack_code', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('cis', 'cis', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('sscc', 'sscc', [xsaSimpleObject], '', 1, 255);
end;

procedure Tunit_pack_Document_pack_content.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tunit_pack_Document_pack_content.Destroy;
begin
  inherited Destroy;
end;

constructor Tunit_pack_Document_pack_content.Create;
begin
  inherited Create;
end;

end.
