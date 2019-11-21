{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании

  Приказ Федеральной налоговой службы от 27 августа 2019 г. № ММВ-7-15/423@
  "Об утверждении формата представления документа о приемке материальных ценностей
   и (или) расхождениях, выявленных при их приемке, в электронной форме"

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

unit et_torg2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AbstractExchangeFileUnit, xmlobject;

type

  { TAcceptanceDocument }

  TAcceptanceDocument = class(TXmlSerializationObject) //%Таблица 5.4
  private
    FKND: string;
    procedure SetKND(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
(*
    Дата формирования файла обмена информации покупателя
    ДатаИнфПок
    А
    Т(=10)
    О
    Типовой элемент <ДатаТип>
    Время формирования файла обмена информации покупателя
    ВремИнфПок
    А
    Т(=8)
    О
    Типовой элемент <ВремяТип>
    Наименование документа по событию (факту хозяйственной жизни)
    ПоФактХЖ
    А
    Т( 1-255)
    О
    Принимает значение: "Документ о приемке ценностей и (или) расхождениях, выявленных при их приемке"
    Наименование документа, определенное организацией (согласованное сторонами сделки)
    НаимДокОпр
    А
    Т(1-255)
    О
    Документ о приемке ценностей и (или) расхождениях или иное
    Наименование экономического субъекта - составителя файла обмена информации покупателя
    НаимЭконСубСост
    А
    Т(1-1000)
    О

    Основание, по которому экономический субъект является составителем файла обмена информации покупателя
    ОснДоверОргСост
    А
    Т(1-120)
    Н
    Обязателен, если составитель информации покупателя не является покупателем
    Обстоятельства составления документа
    ОбстСостДок
    А
    Т(=1)
    НК
    Принимает значение 1 | 2, где:
    1 - документ составлен при приемке ценностей от продавца (отправителя, перевозчика);
    2 - документ составлен после приемки ценностей от продавца (отправителя, перевозчика) в течение согласованного сторонами периода
    Назначение и подписанты дополнительных сведений
    НазнДопСв
    А
    Т(1-2000)
    Н
    Обязателен, если ИнфДопСв равно 4, или равно 6, или равно 8.
    Могут указываться назначение и подписанты формируемых дополнительных сведений
    Дата и номер документа о приемке и (или) расхождениях
    ИдентДокПР
    С

    О
    Состав элемента представлен в таблице 5.5
    Исправление документа о приемке и (или) расхождениях
    ИспрДокПР
    С

    Н
    Состав элемента представлен в таблице 5.6
    Содержание события (факта хозяйственной жизни (1)) - сведения об обстоятельствах приемки
    СодФХЖ1
    С

    О
    Состав элемента представлен в таблице 5.7
    Содержание события (факта хозяйственной жизни (2)) - сведения о факте приемки и (или) о расхождениях
    СодФХЖ2
    С

    О
    Состав элемента представлен в таблице 5.18
    Информация о формировании дополнительных сведений к документу
    ИнфДопСв
    П
    Т(1-2)
    ОКМ
    Принимает значение 1 по умолчанию или 2 | 3 | 4 | 5 | 6 | 7 | 8, где:
    1 - к документу дополнительные сведения не формируются;
    2 - к документу формируются дополнительные сведения об оприходовании ценностей покупателем (в том числе на склад), подписанные ответственным лицом покупателя (уполномоченным покупателем лицом);
    3 - к документу формируются дополнительные сведения о его утверждении, подписанные ответственным лицом покупателя (уполномоченным покупателем лицом);
    4 - к документу формируются иные дополнительные сведения, подписанные ответственным лицом покупателя (уполномоченным покупателем лицом);
    5 - к документу формируются дополнительные сведения о согласии (несогласии) с результатами приемки, подписанные ответственным лицом со стороны продавца (уполномоченным продавцом лицом);
    6 - к документу формируются иные дополнительные сведения, подписанные ответственным лицом со стороны продавца (уполномоченным продавцом лицом);
    7 - к документу формируются дополнительные сведения о согласии (несогласии) с результатами приемки, подписанные ответственным лицом со стороны перевозчика (уполномоченным перевозчиком лицом);
    8 - к документу формируются иные дополнительные сведения, подписанные ответственным лицом со стороны перевозчика (уполномоченным перевозчиком лицом)
    Сведения о лице, подписавшем файл обмена информации покупателя в электронной форме
    Подписант
    С

    ОМ
    Состав элемента представлен в таблице 5.23
*)
  published
    property KND:string read FKND write SetKND;
  end;

  { TSellerExchangeInformation }

  TSellerExchangeInformation = class(TXmlSerializationObject) //%Таблица 5.3
  private
    FFullName: string;
    FIdentifierSenderOperator: string;
    FInn: string;
    procedure SetFullName(AValue: string);
    procedure SetIdentifierSenderOperator(AValue: string);
    procedure SetInn(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
  public
  published
    property FullName:string read FFullName write SetFullName;
    property Inn:string read FInn write SetInn;
    property IdentifierSenderOperator:string read FIdentifierSenderOperator write SetIdentifierSenderOperator;
  end;

  { TParticipantsInformation }

  TParticipantsInformation = class(TXmlSerializationObject)  //%Таблица 5.2
  private
    FRecipientInfo: string;
    FSellerExchangeInformation: TSellerExchangeInformation;
    FSenderInfo: string;
    procedure SetRecipientInfo(AValue: string);
    procedure SetSenderInfo(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property SenderInfo:string read FSenderInfo write SetSenderInfo;
    property RecipientInfo:string read FRecipientInfo write SetRecipientInfo;
    property SellerExchangeInformation:TSellerExchangeInformation read FSellerExchangeInformation;
  end;

  { TTorg2ExchangeFile }

  TTorg2ExchangeFile = class(TAbstractExchangeFile)   //%Таблица 5.1
  private
    FDocument: TAcceptanceDocument;
    FParticipantsInformation: TParticipantsInformation;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ParticipantsInformation:TParticipantsInformation read FParticipantsInformation;
    property Document:TAcceptanceDocument read FDocument;
  end;

implementation

{ TAcceptanceDocument }

procedure TAcceptanceDocument.SetKND(AValue: string);
begin
  if FKND=AValue then Exit;
  FKND:=AValue;
  ModifiedProperty('KND');
end;

procedure TAcceptanceDocument.InternalRegisterPropertys;
begin
  RegisterProperty('KND', 'КНД', 'ОК', 'Код документа по КНД', 7, 7);
end;

procedure TAcceptanceDocument.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

{ TSellerExchangeInformation }

procedure TSellerExchangeInformation.SetFullName(AValue: string);
begin
  if FFullName=AValue then Exit;
  FFullName:=AValue;
  ModifiedProperty('FullName');
end;

procedure TSellerExchangeInformation.SetIdentifierSenderOperator(AValue: string
  );
begin
  if FIdentifierSenderOperator=AValue then Exit;
  FIdentifierSenderOperator:=AValue;
  ModifiedProperty('IdentifierSenderOperator');
end;

procedure TSellerExchangeInformation.SetInn(AValue: string);
begin
  if FInn=AValue then Exit;
  FInn:=AValue;
  ModifiedProperty('Inn');
end;

procedure TSellerExchangeInformation.InternalRegisterPropertys;
begin
  RegisterProperty('FullName', 'НаимОрг', 'О', 'Наименование', 1, 1000);
  RegisterProperty('Inn', 'ИННЮЛ', 'О', 'ИНН', 10, 10);
  RegisterProperty('IdentifierSenderOperator', 'ИдЭДО', 'О', 'Идентификатор оператора электронного документооборота отправителя файла обмена счета-фактуры (информации продавца)', 3, 3);
end;

{ TParticipantsInformation }

procedure TParticipantsInformation.SetRecipientInfo(AValue: string);
begin
  if FRecipientInfo=AValue then Exit;
  FRecipientInfo:=AValue;
  ModifiedProperty('RecipientInfo');
end;

procedure TParticipantsInformation.SetSenderInfo(AValue: string);
begin
  if FSenderInfo=AValue then Exit;
  FSenderInfo:=AValue;
  ModifiedProperty('SenderInfo');
end;

procedure TParticipantsInformation.InternalRegisterPropertys;
begin
  RegisterProperty('SenderInfo', 'ИдОтпр', 'О', 'Идентификатор участника документооборота - отправителя файла обмена счета-фактуры (информации продавца)', 4, 46);
  RegisterProperty('RecipientInfo', 'ИдПол', 'О', 'Идентификатор участника документооборота - получателя файла обмена счета-фактуры (информации продавца)', 4, 46);
  RegisterProperty('SellerExchangeInformation', 'СвОЭДОтпр', 'Н', 'Сведения об операторе электронного документооборота отправителя файла обмена счета-фактуры (информации продавца)', -1, -1);
end;

procedure TParticipantsInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FSellerExchangeInformation:=TSellerExchangeInformation.Create;
end;

destructor TParticipantsInformation.Destroy;
begin
  FreeAndNil(FSellerExchangeInformation);
  inherited Destroy;
end;

{ TTorg2ExchangeFile }

procedure TTorg2ExchangeFile.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('ParticipantsInformation', 'СвУчДокОбор', 'О', 'Сведения об участниках электронного документооборота', -1, -1);
  RegisterProperty('Document', 'Документ', 'О', 'Документ о приемке и (или) расхождениях (информация покупателя) (Документ)', -1, -1);
end;

procedure TTorg2ExchangeFile.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FParticipantsInformation:=TParticipantsInformation.Create;
  FDocument:=TAcceptanceDocument.Create;
end;

destructor TTorg2ExchangeFile.Destroy;
begin
  FreeAndNil(FParticipantsInformation);
  FreeAndNil(FDocument);
  inherited Destroy;
end;

end.

