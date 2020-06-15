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

unit LP_base_types_v2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject;

type
  //IPv4. Запись в виде четырёх десятичных чисел (от 0 до 255), разделённых точками.
  TIPv4Address_type = String;
  //IPv6. Запись в виде восьми четырёхзначных шестнадцатеричных чисел (групп по четыре символа), разделённых
  //двоеточием.
  TIPv6Address_type = String;
  //Текстовое значение не более 255 символов
  Tstring255_type = String;
  //Текстовое значение не более 1000 символов
  Tstring1000_type = String;
  //Справочник типов участников
  //Участник оборота товаров - TRADE_PARTICIPANT
  //Оператор ИС МП - OPERATOR
  //ЦЭМ - LABELLING_CENTER
  //ОГВ - OGV
  //Эмитент - EMITENT
  Tparticipant_type = String;
  //ИНН участника оборота
  Tinn_type = String;
  //КПП участника оборота
  Tkpp_type = String;
  //Значение даты в формате ДД.ММ.ГГГГ.
  Tdate_type = String;
  //Значение даты и времени, а так же смещение, относительно времени в формате UTC
  Tdatetimeoffset_type = TDateTime;
  //Справочник видов оборота товаров
  //Продажа-покупка - BUYING_AND_SELLING
  //Комиссия - COMMISSION
  //Агент - AGENT
  //Продажа комиссионером - COMMISSIONAIRE_SALE
  //Подряд - CONTRACT
  Tturnover_enum_type = String;
  //Стоимость/Налог
  Tprice_type = Double;
  //GUID (Globally Unique Identifier)
  Tguid_type = String;
  //Регистрационный токен СУЗ
  Ttoken_type = String;
  //Порт TCP/IP
  Ttcpip_port_type = Int64;
  //Код товарной номенклатуры (4 знака)
  Ttnved_code_4_type = Int64;
  //Код товарной номенклатуры (2 знака)
  Ttnved_code_2_type = Int64;
  //Код ТН ВЭД ЕАС товара
  Ttnved_code_type = Int64;
  //Справочник причин вывода из оборота
  //Розничная продажа - RETAIL
  //Экспорт в страны ЕАЭС - EEC_EXPORT
  //Экспорт за пределы стран ЕАЭС - BEYOND_EEC_EXPORT
  //Возврат физическому лицу - RETURN
  //Продажа по образцам, дистанционный способ продажи - REMOTE_SALE
  //Утрата или повреждение - DAMAGE_LOSS
  //Уничтожение - DESTRUCTION
  //Конфискация - CONFISCATION
  //Ликвидация предприятия - LIQUIDATION
  Twithdrawal_type = String;
  //Справочник причин вывода из оборота при отгрузке
  //Безвозмездная передача - DONATION
  //Приобретение гос.предприятием - STATE_ENTERPRISE
  //Использование для собственных нужд покупателем - NO_RETAIL_USE
  Twithdrawal_shipment_type = String;
  //Справочник наименований первичных документа
  //Кассовый чек - RECEIPT
  //Кассовый чек коррекции - CORRECTION_RECEIPT
  //Товарный чек - SALES_RECEIPT
  //Товарная накладная - CONSIGNMENT_NOTE
  //Универсальный передаточный документ - UTD
  //Таможенная декларация на товары - CUSTOMS_DECLARATION
  //Акт уничтожения (утраты/утилизации) - DESTRUCTION_ACT
  //Прочее - OTHER
  Tprimary_document_name_type = String;
  //Уникальный идентификатор транспортной упаковки или товара
  Tgs1_uit_uitu_type = String;
  //GTIN
  Tgtin_type = String;
  //Справочник типов производственного заказа
  //Собственное производство - OWN_PRODUCTION
  //Контрактное производство - CONTRACT_PRODUCTION
  Tproduction_order_type = String;
  //Справочник видов документов, подтверждающих соответствие
  //Сертификат соответствия - CONFORMITY_CERTIFICATE
  //Декларация соответствия - CONFORMITY_DECLARATION
  Tcertificate_type_type = String;
  //Серийный номер
  Tsn_type = String;
  //Справочник способов ввода товаров в оборот
  //Произведен в РФ - PRODUCED_IN_RF
  //Ввезен в РФ - IMPORTED_INTO_RF
  Trelease_method_type = String;
  //Справочник кодов принятого решения
  //10 - Выпуск товаров разрешен
  //11 - Выпуск товаров при условии обеспечения исполнения обязанности по уплате таможенных пошлин, налогов, специальных,
  //антидемпинговых, компенсационных пошлин, за исключением выпуска товаров, поименованного в позициях с кодами 12 и 13
  //12 - Выпуск товаров с особенностями, предусмотренными статьей 121 Таможенного кодекса Евразийского экономического союза
  //13 - Выпуск товаров с особенностями, предусмотренными статьей 122 Таможенного кодекса Евразийского экономического союза
  //14 - Выпуск товаров с особенностями, предусмотренными статьей 123 Таможенного кодекса Евразийского экономического союза
  //20 - Условный выпуск товаров
  Tdecision_code_type = Longint;
  //Справочник причин перемаркировки
  //Испорчено либо утеряно СИ с КМ - KM_SPOILED
  //Выявлены ошибки описания товара - DESCRIPTION_ERRORS
  Tremark_cause_type = String;
  //Спроавочник причин списания кодов маркировки
  //Испорчен - KM_SPOILED
  //Утерян - KM_LOST
  //Уничтожен - KM_DESTROYED
  Tkm_cancellation_reason_type = String;
  //Спроавочник типов трансформации транспортной упаковкиИзъятие - REMOVING
  //Добавление - ADDING
  Treaggregation_type_type = String;
  //КИТУ (код идентификации транспортной упаковки)
  Tkitu_type = String;
  //КИ (код идентификации)
  Tkit_type = String;
  //КИ (код идентификации)
  Tki_type = String;
  //Способ изготовления
  //Самостоятельно - OWN
  //ЦЭМ - LC
  Tproduction_method_type = String;
  //Способ получения кодов маркировки
  //На физическом носителе - PHYSICAL_MEDIA
  //В электронном виде - ELECTRONIC
  Treception_method_type = String;
  //Способ формирования серийного номера
  //Самостоятельно - OWN
  //Оператором - OPERATOR
  Tsn_method_type = String;
  //Серийный номер сертификата УКЭП
  Tcertificate_sn_type = String;
  //Справочник типов движения товара
  //Приход-расход - INCOME_OUTCOME
  //Возврат - RETURN
  Tdistribution_type_type = String;
  //Справочник типов отгрузки
  //Отгрузка - SHIPMENTПересорт - RETAKE
  //Обмен - EXCHANGE
  Tshipment_type_type = String;
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tchildren_products_list_type = class;
  Tchildren_products_list_type_product = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tchildren_products_list_typeList = specialize GXMLSerializationObjectList<Tchildren_products_list_type>;
  Tchildren_products_list_type_productList = specialize GXMLSerializationObjectList<Tchildren_products_list_type_product>;

  {  Tfias_address_type  }
  //Адрес в формате ФИАС
  Tfias_address_type = class(TXmlSerializationObject)
  private
    Faoguid:Taoguid;
    Fhouseguid:Thouseguid;
    Fflat:Tflat;
    procedure Setaoguid( AValue:Taoguid);
    procedure Sethouseguid( AValue:Thouseguid);
    procedure Setflat( AValue:Tflat);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Глобальный уникальный идентификатор адресного объекта
    property aoguid:Taoguid read Faoguid write Setaoguid;
    //Глобальный уникальный идентификатор дома. Обязателен при наличии
    property houseguid:Thouseguid read Fhouseguid write Sethouseguid;
    //Квартира. Обязателен при наличии
    property flat:Tflat read Fflat write Setflat;
  end;

  {  Tchildren_products_list_type  }
  Tchildren_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tchildren_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список товаров в составе транспортной упаковки
    property product:Tchildren_products_list_type_productList read Fproduct;
  end;

  {  Tchildren_products_list_type_product  }
  Tchildren_products_list_type_product = class(TXmlSerializationObject)
  private
    Fkit:Tkit_type;
    Fkitu:Tkitu_type;
    Faccept_type:Boolean;
    Fchildren_products_list:Tchildren_products_list_type;
    procedure Setkit( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setaccept_type( AValue:Boolean);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property kit:Tkit_type read Fkit write Setkit;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Товар принят
    property accept_type:Boolean read Faccept_type write Setaccept_type;
    //Список товаров в составе транспортной упаковки
    property children_products_list:Tchildren_products_list_type read Fchildren_products_list;
  end;

implementation

  {  Tfias_address_type  }
procedure Tfias_address_type.Setaoguid(AValue: Taoguid);
begin
  CheckStrMinSize('aoguid', AValue);
  CheckStrMaxSize('aoguid', AValue);
  Faoguid:=AValue;
  ModifiedProperty('aoguid');
end;

procedure Tfias_address_type.Sethouseguid(AValue: Thouseguid);
begin
  CheckStrMinSize('houseguid', AValue);
  CheckStrMaxSize('houseguid', AValue);
  Fhouseguid:=AValue;
  ModifiedProperty('houseguid');
end;

procedure Tfias_address_type.Setflat(AValue: Tflat);
begin
  CheckStrMinSize('flat', AValue);
  CheckStrMaxSize('flat', AValue);
  Fflat:=AValue;
  ModifiedProperty('flat');
end;

procedure Tfias_address_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('aoguid', 'aoguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('houseguid', 'houseguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('flat', 'flat', [xsaSimpleObject], '', 1, 20);
end;

procedure Tfias_address_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tfias_address_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tfias_address_type.Create;
begin
  inherited Create;
end;

  {  Tchildren_products_list_type  }
procedure Tchildren_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tchildren_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tchildren_products_list_type_productList.Create;
end;

destructor Tchildren_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tchildren_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tchildren_products_list_type_product  }
procedure Tchildren_products_list_type_product.Setkit(AValue: Tkit_type);
begin
  CheckStrMinSize('kit', AValue);
  CheckStrMaxSize('kit', AValue);
  Fkit:=AValue;
  ModifiedProperty('kit');
end;

procedure Tchildren_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tchildren_products_list_type_product.Setaccept_type(AValue: Boolean);
begin
  Faccept_type:=AValue;
  ModifiedProperty('accept_type');
end;

procedure Tchildren_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('kit', 'kit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('accept_type', 'accept_type', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tchildren_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tchildren_products_list_type.Create;
end;

destructor Tchildren_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tchildren_products_list_type_product.Create;
begin
  inherited Create;
end;

end.