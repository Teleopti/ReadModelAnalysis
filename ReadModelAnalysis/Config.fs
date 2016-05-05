module Config

type ConfigValues = {
    pathToNhibMappings : string;
    pathToStoredProcedures : string;
    pathToInfraClasses : string;
    pathToDomainClasses : string;
}

let configValues = {
    pathToNhibMappings =  @"C:\Teleopti\Domain";
    pathToStoredProcedures =  @"C:\Teleopti\Database\TeleoptiCCC7\Programmability\03StoredProcedures" ; 
    pathToInfraClasses = @"C:\Teleopti\Infrastructure";
    pathToDomainClasses = @"C:\Teleopti\Domain";
}