module EmulatorLib where
import qualified DataPath(simulate)
import DataPath(outStorage, ioDev, DataPath)

simulate = DataPath.simulate

getOutMem = outStorage.ioDev