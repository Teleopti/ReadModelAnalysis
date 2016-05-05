module Config

type ConfigValues = {
    pathToNhibMappings : string;
    pathToStoredProcedures : string;
    pathToInfraClasses : string;
    pathToDomainClasses : string;
    pathToWebClasses : string;
    pathToDomainAssembly : string;
    domainNsPrefix : string;
}

let configValues = {
    pathToNhibMappings =  @"C:\Teleopti\Domain";
    pathToStoredProcedures =  @"C:\Teleopti\Database\TeleoptiCCC7\Programmability\03StoredProcedures" ; 
    pathToInfraClasses = @"C:\Teleopti\Infrastructure";
    pathToDomainClasses = @"C:\Teleopti\Domain";
    pathToWebClasses = @"C:\Teleopti\Teleopti.Ccc.Web\Teleopti.Ccc.Web";
    pathToDomainAssembly = @"C:\Teleopti\Domain\bin\Debug\Teleopti.Ccc.Domain.dll";
    domainNsPrefix = @"Teleopti.Ccc.Domain.";
}