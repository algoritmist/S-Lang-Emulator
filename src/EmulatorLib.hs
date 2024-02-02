module EmulatorLib where
import           DataPath (DataPath, ioDev, outStorage)
import qualified DataPath (simulate)

simulate = DataPath.simulate

getOutMem = outStorage.ioDev
