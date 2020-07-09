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
  Classes, SysUtils, xmlobject, AbstractSerializationObjects;

type
  //IPv4. Запись в виде четырёх десятичных чисел (от 0 до 255), разделённых точками.
  TIPv4Address_type = String;
  //IPv6. Запись в виде восьми четырёхзначных шестнадцатеричных чисел (групп по четыре символа), разделённых двоеточием.
  TIPv6Address_type = String;
  //Текстовое значение не более 255 символов
  Tstring255_type = String;
  //Текстовое значение не более 200 символов
  Tstring200_type = String;
  //Текстовое значение не более 1000 символов
  Tstring1000_type = String;
  //Текстовое значение не более 50 символов
  Tstring50_type = String;
  //Текстовое значение не более 1024 символов
  Tstring1024_type = String;
  //Текстовое значение не более 128 символов
  Tstring128_type = String;
  //Текстовое значение не более 70 символов
  Tstring70_type = String;
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
  //Продажа - SELLING
  //Комиссия - COMMISSION
  //Агент - AGENT
  //Продажа комиссионером - COMMISSIONAIRE_SALE
  //Подряд - CONTRACT
  Tturnover_enum_type = String;
  //Пол обуви
  //Мужской - MALE
  //Женский - FEMALE
  //Детская - BABY
  //Унисекс - UNISEX
  Tproduct_gender_type = String;
  //Стоимость/Налог
  Tprice_type = Double;
  //GUID (Globally Unique Identifier)
  Tguid_type = String;
  //Регистрационный токен СУЗ
  Ttoken_type = String;
  //Порт TCP/IP
  Ttcpip_port_type = Int64;
  //Размер в штихмассовой системе
  Tshoe_size = Double;
  //Код товарной номенклатуры (4 знака)
  Ttnved_code_4_type = String;
  //Код товарной номенклатуры (2 знака)
  Ttnved_code_2_type = String;
  //Код ТН ВЭД ЕАС товара
  Ttnved_code_type = String;
  //Справочник причин вывода из оборота
  //Розничная продажа - RETAIL
  //Экспорт в страны ЕАЭС - EEC_EXPORT
  //Экспорт за пределы стран ЕАЭС - BEYOND_EEC_EXPORT
  //Возврат физическому лицу - RETURN
  //Продажа по образцам, дистанционный способ продажи - REMOTE_SALE
  //Утрата или повреждение - DAMAGE_LOSS
  //Уничтожение - DESTRUCTION
  //Конфискация - CONFISCATION
  //Использование для собственных нужд предприятия - ENTERPRISE_USE
  //Ликвидация предприятия - LIQUIDATION
  Twithdrawal_type = String;
  //Справочник причин вывода из оборота при отгрузке
  //Безвозмездная передача - DONATION
  //Приобретение гос.предприятием - STATE_ENTERPRISE
  //Использование для собственных нужд покупателем - NO_RETAIL_USE
  Twithdrawal_shipment_type = String;
  //Справочник типов первичных документов для вывода из оборота
  //Кассовый чек - RECEIPT
  //Бланк строгой отчетности - STRICT_REPORTING_FORM
  //Договор - CONTRACT
  //Акт уничтожения (утраты/утилизации) - DESTRUCTION_ACT
  //Товарная накладная - CONSIGNMENT_NOTE
  //Универсальный передаточный документ - UTD
  //Прочее - OTHER
  Twithdraw_primary_document_type_type = String;
  //Справочник типов первичных документов для вывода из оборота
  //Кассовый чек - RECEIPT
  //Товарный чек - SALES_RECEIPT
  //Прочее - OTHER
  Treturn_primary_document_type_type = String;
  //Справочник типов первичных документов для отгрузки
  //Товарная накладная - CONSIGNMENT_NOTE
  //Универсальный передаточный документ - UTD
  //Прочее - OTHER
  Tshipment_primary_document_type_type = String;
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
  //Декларация о соответствии - CONFORMITY_DECLARATION
  Tcertificate_type_type = String;
  //Серийный номер
  Tsn_type = String;
  //Справочник способов ввода товаров в оборот
  //Произведен в РФ - PRODUCED_IN_RF
  //Ввезен в РФ - IMPORTED_INTO_RF
  //Остатки - OSTATKI
  //ТрансГран - CROSSBORDER
  Trelease_method_type = String;
  //Справочник кодов принятого решения
  //10 - Выпуск товаров разрешен
  //11 - Выпуск товаров при условии обеспечения исполнения обязанности по уплате таможенных пошлин, налогов, специальных, антидемпинговых, компенсационных пошлин, за исключением выпуска товаров, поименованного в позициях с кодами 12 и 13
  //12 - Выпуск товаров с особенностями, предусмотренными статьей 121 Таможенного кодекса Евразийского экономического союза
  //13 - Выпуск товаров с особенностями, предусмотренными статьей 122 Таможенного кодекса Евразийского экономического союза
  //14 - Выпуск товаров с особенностями, предусмотренными статьей 123 Таможенного кодекса Евразийского экономического союза
  //20 - Условный выпуск товаров
  Tdecision_code_type = Longint;
  //Испорчено либо утеряно СИ с КМ - KM_SPOILED_OR_LOST
  //Выявлены ошибки описания товара - DESCRIPTION_ERRORS
  //Возврат товаров с поврежденным СИ/без СИ при розничной реализации - RETAIL_RETURN
  //Возврат товаров с поврежденным СИ/без СИ при дистанционном способе продажи - REMOTE_SALE_RETURN
  Tremark_cause_type = String;
  //Спроавочник причин списания кодов маркировки
  //Испорчен - KM_SPOILED
  //Утерян - KM_LOST
  //Уничтожен - KM_DESTROYED
  Tkm_cancellation_reason_type = String;
  //Спроавочник типов трансформации транспортной упаковки
  //Изъятие - REMOVING
  //Добавление - ADDING
  Treaggregation_type_type = String;
  //Спроавочник типов трансформации упаковки
  //Изъятие - REMOVING
  //Добавление - ADDING
  Ttransformation_type_type = String;
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
  //Справочник видов ввода товарв в оборот
  //Собственное производство РФ - OWN_PRODUCTION
  //Контрактное производство РФ - CONTRACT_PRODUCTION
  //Производство вне ЕАЭС - BEYOND_EEC_PRODUCTION
  //Производство в ЕАЭС - EEC_PRODUCTION
  //Ввод полученных от физ.лиц товаров - INDIVIDUALS
  //Ввод остатков товара - OSTATKI
  Tvvod_type_type = String;
  //Код страны ОКСМ
  Toksm_type = String;
  //Код страны ОКСМ для ввода в оборот при трансграничной торговле
  //Армения - 051
  //Беларусь - 112
  //Казахстан - 398
  //Киргизия - 417
  Toksm_cb_type = String;
  //Буквенный код идентификации страны в соответствии с ОКСМ (альфа-2)
  Toksm_alpha2_type = String;
  //Справочник видов возврата
  //Возврат при розничной реализации - RETAIL_RETURN
  //Возврат при дистанционном способе реализации - REMOTE_SALE_RETURN
  Treturn_type_type = String;
  //Код товарной номенклатуры (2 знака) для остатков
  Ttnved_code_2_ost_type = Int64;
  //Возрастная категория товара
  //Детское - BABY
  //Взрослое - ADULT
  //Без возраста - NO_AGE
  Tage_category_type = String;
  //Регистрационный номер ДТ
  Tdeclaration_number_type = String;
  //Агрегированный таможенный код
  Tatk_type = String;
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tshipment_children_products_list_type = class;
  Tshipment_children_products_list_type_product = class;
  Tacceptance_children_products_list_type = class;
  Tacceptance_children_products_list_type_product = class;
  Tvvod_children_products_list_type = class;
  Tvvod_children_products_list_type_product = class;
  Tvvod_ind_children_products_list_type = class;
  Tvvod_ind_children_products_list_type_product = class;
  Tvvod_ost_children_products_list_type = class;
  Tvvod_ost_children_products_list_type_product = class;
  Tvvod_crossborder_products_list_type = class;
  Tvvod_crossborder_products_list_type_product = class;
  Tvvod_fts_children_list_type = class;
  Tvvod_fts_children_list_type_product = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tshipment_children_products_list_typeList = specialize GXMLSerializationObjectList<Tshipment_children_products_list_type>;
  Tshipment_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tshipment_children_products_list_type_product>;
  Tacceptance_children_products_list_typeList = specialize GXMLSerializationObjectList<Tacceptance_children_products_list_type>;
  Tacceptance_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tacceptance_children_products_list_type_product>;
  Tvvod_children_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_children_products_list_type>;
  Tvvod_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_children_products_list_type_product>;
  Tvvod_ind_children_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_ind_children_products_list_type>;
  Tvvod_ind_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_ind_children_products_list_type_product>;
  Tvvod_ost_children_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_ost_children_products_list_type>;
  Tvvod_ost_children_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_ost_children_products_list_type_product>;
  Tvvod_crossborder_products_list_typeList = specialize GXMLSerializationObjectList<Tvvod_crossborder_products_list_type>;
  Tvvod_crossborder_products_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_crossborder_products_list_type_product>;
  Tvvod_fts_children_list_typeList = specialize GXMLSerializationObjectList<Tvvod_fts_children_list_type>;
  Tvvod_fts_children_list_type_productList = specialize GXMLSerializationObjectList<Tvvod_fts_children_list_type_product>;

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

  {  Tshipment_children_products_list_type  }
  Tshipment_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tshipment_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ/КИТУ в составе транспортной упаковки (отгрузка)
    property product:Tshipment_children_products_list_type_productList read Fproduct;
  end;

  {  Tshipment_children_products_list_type_product  }
  Tshipment_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tki_type;
    Fkitu:Tkitu_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    Fchildren_products_list:Tshipment_children_products_list_type;
    procedure Setki( AValue:Tki_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tki_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Цена за единицу
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма НДС
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
    //Список КИ\КИТУ составе транспортной упаковки (отгрузка)
    property children_products_list:Tshipment_children_products_list_type read Fchildren_products_list;
  end;

  {  Tacceptance_children_products_list_type  }
  Tacceptance_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tacceptance_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки (приемка)
    property product:Tacceptance_children_products_list_type_productList read Fproduct;
  end;

  {  Tacceptance_children_products_list_type_product  }
  Tacceptance_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tki_type;
    Fkitu:Tkitu_type;
    Faccept_type:Boolean;
    Fchildren_products_list:Tacceptance_children_products_list_type;
    procedure Setki( AValue:Tki_type);
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
    property ki:Tki_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Товар принят
    property accept_type:Boolean read Faccept_type write Setaccept_type;
    //Список КИ\КИТУ в составе транспортной упаковки (приемка)
    property children_products_list:Tacceptance_children_products_list_type read Fchildren_products_list;
  end;

  {  Tvvod_children_products_list_type  }
  Tvvod_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот
    property product:Tvvod_children_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_children_products_list_type_product  }
  Tvvod_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Fproduct_date:Tdate_type;
    Ftnved_code:Ttnved_code_type;
    Fcertificate_type:Tcertificate_type_type;
    Fcertificate_number:Tstring255_type;
    Fcertificate_date:Tdate_type;
    Fvvod_children_products_list:Tvvod_children_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setproduct_date( AValue:Tdate_type);
    procedure Settnved_code( AValue:Ttnved_code_type);
    procedure Setcertificate_type( AValue:Tcertificate_type_type);
    procedure Setcertificate_number( AValue:Tstring255_type);
    procedure Setcertificate_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Дата производства
    property product_date:Tdate_type read Fproduct_date write Setproduct_date;
    //Код ТН ВЭД ЕАС товара
    property tnved_code:Ttnved_code_type read Ftnved_code write Settnved_code;
    //Вид документа, подтверждающего соответствие
    property certificate_type:Tcertificate_type_type read Fcertificate_type write Setcertificate_type;
    //Номер документа, подтверждающего соответствие
    property certificate_number:Tstring255_type read Fcertificate_number write Setcertificate_number;
    //Дата документа, подтверждающего соответствие
    property certificate_date:Tdate_type read Fcertificate_date write Setcertificate_date;
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот
    property vvod_children_products_list:Tvvod_children_products_list_type read Fvvod_children_products_list;
  end;

  {  Tvvod_ind_children_products_list_type  }
  Tvvod_ind_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_ind_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (полученных от физических лиц)
    property product:Tvvod_ind_children_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_ind_children_products_list_type_product  }
  Tvvod_ind_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Fproduct_receiving_date:Tdate_type;
    Fvvod_ind_children_products_list:Tvvod_ind_children_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setproduct_receiving_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Дата получения товара
    property product_receiving_date:Tdate_type read Fproduct_receiving_date write Setproduct_receiving_date;
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (полученных от физических лиц)
    property vvod_ind_children_products_list:Tvvod_ind_children_products_list_type read Fvvod_ind_children_products_list;
  end;

  {  Tvvod_ost_children_products_list_type  }
  Tvvod_ost_children_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_ost_children_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (остатки)
    property product:Tvvod_ost_children_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_ost_children_products_list_type_product  }
  Tvvod_ost_children_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Fvvod_ost_children_products_list:Tvvod_ost_children_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (остатки)
    property vvod_ost_children_products_list:Tvvod_ost_children_products_list_type read Fvvod_ost_children_products_list;
  end;

  {  Tvvod_crossborder_products_list_type  }
  Tvvod_crossborder_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_crossborder_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ\КИТУ в составе транспортной упаковки для ввода в оборот (трансграничная торговля)
    property product:Tvvod_crossborder_products_list_type_productList read Fproduct;
  end;

  {  Tvvod_crossborder_products_list_type_product  }
  Tvvod_crossborder_products_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Ftnved_code:Ttnved_code_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    Fcertificate_type:Tcertificate_type_type;
    Fcertificate_number:Tstring255_type;
    Fcertificate_date:Tdate_type;
    Fchildren_products_list:Tvvod_crossborder_products_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Settnved_code( AValue:Ttnved_code_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
    procedure Setcertificate_type( AValue:Tcertificate_type_type);
    procedure Setcertificate_number( AValue:Tstring255_type);
    procedure Setcertificate_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Код ТН ВЭД ЕАС товара
    property tnved_code:Ttnved_code_type read Ftnved_code write Settnved_code;
    //Цена за единицу
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма НДС
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
    //Вид документа, подтверждающего соответствие
    property certificate_type:Tcertificate_type_type read Fcertificate_type write Setcertificate_type;
    //Номер документа, подтверждающего соответствие
    property certificate_number:Tstring255_type read Fcertificate_number write Setcertificate_number;
    //Дата документа, подтверждающего соответствие
    property certificate_date:Tdate_type read Fcertificate_date write Setcertificate_date;
    //Список КИ/КИТУ в составе транспортной упаковки для ввода в оборот (трансграничная торговля)
    property children_products_list:Tvvod_crossborder_products_list_type read Fchildren_products_list;
  end;

  {  Tvvod_fts_children_list_type  }
  Tvvod_fts_children_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_fts_children_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список КИ/КИТУ в составе КИТУ/АТК для ввода в оборот импортных товаров с ФТС
    property product:Tvvod_fts_children_list_type_productList read Fproduct;
  end;

  {  Tvvod_fts_children_list_type_product  }
  Tvvod_fts_children_list_type_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    Fcolor:Tstring1024_type;
    Fproduct_size:Tshoe_size;
    Fvvod_fts_children_list_type:Tvvod_fts_children_list_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setcolor( AValue:Tstring1024_type);
    procedure Setproduct_size( AValue:Tshoe_size);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ (код идентификации)
    property ki:Tkit_type read Fki write Setki;
    //КИТУ (код идентификации транспортной упаковки)
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Цвет. Параметр обязателен для ТГ "Обувь"
    property color:Tstring1024_type read Fcolor write Setcolor;
    //Размер в штихмассовой системе. Параметр обязателен для ТГ "Обувь"
    property product_size:Tshoe_size read Fproduct_size write Setproduct_size;
    //Список КИ/КИТУ в составе КИТУ/АТК для ввода в оборот импортных товаров с ФТС
    property vvod_fts_children_list_type:Tvvod_fts_children_list_type read Fvvod_fts_children_list_type;
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

  {  Tshipment_children_products_list_type  }
procedure Tshipment_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tshipment_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tshipment_children_products_list_type_productList.Create;
end;

destructor Tshipment_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tshipment_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tshipment_children_products_list_type_product  }
procedure Tshipment_children_products_list_type_product.Setki(AValue: Tki_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tshipment_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tshipment_children_products_list_type_product.Setcost(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('cost', AValue);
  Fcost:=AValue;
  ModifiedProperty('cost');
end;

procedure Tshipment_children_products_list_type_product.Setvat_value(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('vat_value', AValue);
  Fvat_value:=AValue;
  ModifiedProperty('vat_value');
end;

procedure Tshipment_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('cost', 'cost', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tshipment_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tshipment_children_products_list_type.Create;
end;

destructor Tshipment_children_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tshipment_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tacceptance_children_products_list_type  }
procedure Tacceptance_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tacceptance_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tacceptance_children_products_list_type_productList.Create;
end;

destructor Tacceptance_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tacceptance_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tacceptance_children_products_list_type_product  }
procedure Tacceptance_children_products_list_type_product.Setki(AValue: Tki_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tacceptance_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tacceptance_children_products_list_type_product.Setaccept_type(AValue: Boolean);
begin
  Faccept_type:=AValue;
  ModifiedProperty('accept_type');
end;

procedure Tacceptance_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('accept_type', 'accept_type', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tacceptance_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tacceptance_children_products_list_type.Create;
end;

destructor Tacceptance_children_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tacceptance_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_children_products_list_type  }
procedure Tvvod_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_children_products_list_type_productList.Create;
end;

destructor Tvvod_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_children_products_list_type_product  }
procedure Tvvod_children_products_list_type_product.Setki(AValue: Tkit_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tvvod_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tvvod_children_products_list_type_product.Setproduct_date(AValue: Tdate_type);
begin
  CheckStrMinSize('product_date', AValue);
  CheckStrMaxSize('product_date', AValue);
  Fproduct_date:=AValue;
  ModifiedProperty('product_date');
end;

procedure Tvvod_children_products_list_type_product.Settnved_code(AValue: Ttnved_code_type);
begin
  Ftnved_code:=AValue;
  ModifiedProperty('tnved_code');
end;

procedure Tvvod_children_products_list_type_product.Setcertificate_type(AValue: Tcertificate_type_type);
begin
  CheckLockupValue('certificate_type', AValue);
  Fcertificate_type:=AValue;
  ModifiedProperty('certificate_type');
end;

procedure Tvvod_children_products_list_type_product.Setcertificate_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('certificate_number', AValue);
  CheckStrMaxSize('certificate_number', AValue);
  Fcertificate_number:=AValue;
  ModifiedProperty('certificate_number');
end;

procedure Tvvod_children_products_list_type_product.Setcertificate_date(AValue: Tdate_type);
begin
  CheckStrMinSize('certificate_date', AValue);
  CheckStrMaxSize('certificate_date', AValue);
  Fcertificate_date:=AValue;
  ModifiedProperty('certificate_date');
end;

procedure Tvvod_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('product_date', 'product_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('tnved_code', 'tnved_code', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('certificate_type', 'certificate_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('CONFORMITY_CERTIFICATE');
    P.ValidList.Add('CONFORMITY_DECLARATION');
  P:=RegisterProperty('certificate_number', 'certificate_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_date', 'certificate_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('vvod_children_products_list', 'vvod_children_products_list', [], '', -1, -1);
end;

procedure Tvvod_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fvvod_children_products_list:=Tvvod_children_products_list_type.Create;
end;

destructor Tvvod_children_products_list_type_product.Destroy;
begin
  Fvvod_children_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_ind_children_products_list_type  }
procedure Tvvod_ind_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_ind_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_ind_children_products_list_type_productList.Create;
end;

destructor Tvvod_ind_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_ind_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_ind_children_products_list_type_product  }
procedure Tvvod_ind_children_products_list_type_product.Setki(AValue: Tkit_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tvvod_ind_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tvvod_ind_children_products_list_type_product.Setproduct_receiving_date(AValue: Tdate_type);
begin
  CheckStrMinSize('product_receiving_date', AValue);
  CheckStrMaxSize('product_receiving_date', AValue);
  Fproduct_receiving_date:=AValue;
  ModifiedProperty('product_receiving_date');
end;

procedure Tvvod_ind_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('product_receiving_date', 'product_receiving_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('vvod_ind_children_products_list', 'vvod_ind_children_products_list', [], '', -1, -1);
end;

procedure Tvvod_ind_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fvvod_ind_children_products_list:=Tvvod_ind_children_products_list_type.Create;
end;

destructor Tvvod_ind_children_products_list_type_product.Destroy;
begin
  Fvvod_ind_children_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_ind_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_ost_children_products_list_type  }
procedure Tvvod_ost_children_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_ost_children_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_ost_children_products_list_type_productList.Create;
end;

destructor Tvvod_ost_children_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_ost_children_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_ost_children_products_list_type_product  }
procedure Tvvod_ost_children_products_list_type_product.Setki(AValue: Tkit_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tvvod_ost_children_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tvvod_ost_children_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('vvod_ost_children_products_list', 'vvod_ost_children_products_list', [], '', -1, -1);
end;

procedure Tvvod_ost_children_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fvvod_ost_children_products_list:=Tvvod_ost_children_products_list_type.Create;
end;

destructor Tvvod_ost_children_products_list_type_product.Destroy;
begin
  Fvvod_ost_children_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_ost_children_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_crossborder_products_list_type  }
procedure Tvvod_crossborder_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_crossborder_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_crossborder_products_list_type_productList.Create;
end;

destructor Tvvod_crossborder_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_crossborder_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_crossborder_products_list_type_product  }
procedure Tvvod_crossborder_products_list_type_product.Setki(AValue: Tkit_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tvvod_crossborder_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tvvod_crossborder_products_list_type_product.Settnved_code(AValue: Ttnved_code_type);
begin
  Ftnved_code:=AValue;
  ModifiedProperty('tnved_code');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcost(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('cost', AValue);
  Fcost:=AValue;
  ModifiedProperty('cost');
end;

procedure Tvvod_crossborder_products_list_type_product.Setvat_value(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('vat_value', AValue);
  Fvat_value:=AValue;
  ModifiedProperty('vat_value');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcertificate_type(AValue: Tcertificate_type_type);
begin
  CheckLockupValue('certificate_type', AValue);
  Fcertificate_type:=AValue;
  ModifiedProperty('certificate_type');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcertificate_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('certificate_number', AValue);
  CheckStrMaxSize('certificate_number', AValue);
  Fcertificate_number:=AValue;
  ModifiedProperty('certificate_number');
end;

procedure Tvvod_crossborder_products_list_type_product.Setcertificate_date(AValue: Tdate_type);
begin
  CheckStrMinSize('certificate_date', AValue);
  CheckStrMaxSize('certificate_date', AValue);
  Fcertificate_date:=AValue;
  ModifiedProperty('certificate_date');
end;

procedure Tvvod_crossborder_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('tnved_code', 'tnved_code', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('cost', 'cost', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('certificate_type', 'certificate_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('CONFORMITY_CERTIFICATE');
    P.ValidList.Add('CONFORMITY_DECLARATION');
  P:=RegisterProperty('certificate_number', 'certificate_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_date', 'certificate_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tvvod_crossborder_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tvvod_crossborder_products_list_type.Create;
end;

destructor Tvvod_crossborder_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tvvod_crossborder_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Tvvod_fts_children_list_type  }
procedure Tvvod_fts_children_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_fts_children_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_fts_children_list_type_productList.Create;
end;

destructor Tvvod_fts_children_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_fts_children_list_type.Create;
begin
  inherited Create;
end;

  {  Tvvod_fts_children_list_type_product  }
procedure Tvvod_fts_children_list_type_product.Setki(AValue: Tkit_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tvvod_fts_children_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tvvod_fts_children_list_type_product.Setcolor(AValue: Tstring1024_type);
begin
  CheckStrMinSize('color', AValue);
  CheckStrMaxSize('color', AValue);
  Fcolor:=AValue;
  ModifiedProperty('color');
end;

procedure Tvvod_fts_children_list_type_product.Setproduct_size(AValue: Tshoe_size);
begin
  CheckMinInclusiveValue('product_size', AValue);
  CheckMaxInclusiveValue('product_size', AValue);
  Fproduct_size:=AValue;
  ModifiedProperty('product_size');
end;

procedure Tvvod_fts_children_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('color', 'color', [xsaSimpleObject], '', 1, 1024);
  P:=RegisterProperty('product_size', 'product_size', [xsaSimpleObject], '', -1, -1);
    P.minInclusiveFloat:=14.5;
    P.maxInclusiveFloat:=47;
  P:=RegisterProperty('vvod_fts_children_list_type', 'vvod_fts_children_list_type', [], '', -1, -1);
end;

procedure Tvvod_fts_children_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fvvod_fts_children_list_type:=Tvvod_fts_children_list_type.Create;
end;

destructor Tvvod_fts_children_list_type_product.Destroy;
begin
  Fvvod_fts_children_list_type.Free;
  inherited Destroy;
end;

constructor Tvvod_fts_children_list_type_product.Create;
begin
  inherited Create;
end;

end.
