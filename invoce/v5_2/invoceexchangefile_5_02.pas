unit InvoceExchangeFile_5_02;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc, InvoceExchangeFile, InvoceDocument;

type

  { TExchangeFile5_02 }

  TExchangeFile5_02 = class(TAbstractExchangeFile)   //%Таблица 5.1
  private
    FDocument: TInvoceDocument;
    FParticipantsInformation: TParticipantsInformation;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ParticipantsInformation:TParticipantsInformation read FParticipantsInformation;
    property Document:TInvoceDocument read FDocument;
  end;

implementation

{ TExchangeFile }

procedure TExchangeFile5_02.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('ParticipantsInformation', 'СвУчДокОбор', 'О', 'Сведения об участниках электронного документооборота', -1, -1);
  RegisterProperty('Document', 'Документ', 'О', 'Счет-фактура, или документ об отгрузке товаров (выполнении работ), передаче имущественных прав (документ об оказании услуг), включающий в себя счет-фактуру (информация продавца), или документ об отгрузке товаров (выполнении работ), передаче имущественных прав (документ об оказании услуг) (информация продавца)', -1, -1);
end;

procedure TExchangeFile5_02.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FParticipantsInformation:=TParticipantsInformation.Create;
  FDocument:=TInvoceDocument.Create;
end;

destructor TExchangeFile5_02.Destroy;
begin
  FreeAndNil(FParticipantsInformation);
  FreeAndNil(FDocument);
  inherited Destroy;
end;

end.

