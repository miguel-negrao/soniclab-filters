module Main(main) where

import           Control.Monad                        (when)
import           Control.Monad.IO.Class               (liftIO)

import           Sound.SC3.Server.State.Monad
import           Sound.SC3.Server.State.Monad.Command
import           Sound.SC3.Server.State.Monad.Process
import           Sound.SC3.UGen
--import           Sound.SC3.UGen.Noise.ID
import qualified Sound.OpenSoundControl as OSC

import           System.Environment.Executable
import           System.IO

-- Server config
serveOptions:: FilePath -> ServerOptions
serveOptions d = defaultServerOptions {
        serverProgram = d++"/scsynth",
        numberOfInputBusChannels = 34,
        numberOfOutputBusChannels = 34
}

rTOptions :: RTOptions
rTOptions = defaultRTOptions {
        hardwareDeviceName = Just "JackRouter",
        networkPort = UDPPort 56734
}

run :: FilePath -> Server a -> IO a
run d = withSynth (serveOptions d) rTOptions defaultOutputHandler
--run = withDefaultSynth

-- Impulse responses and Def
is::[Integer]
is = [1..14]++[17..34];

-- | Strict convolution of two continuously changing inputs.
convolution2 :: UGen -> UGen -> UGen -> UGen -> UGen
convolution2 input kernel trigger frameSize = mkOsc AR "Convolution2" [input, kernel, trigger, frameSize] 1

def :: UGen
def = out 0 $ mce $ (convs 34 [1..14]) ++ (mceChannels $ in' 2 AR (34+14) ) ++ (convs (34+16) [17..34])  where
        convs:: UGen -> [Integer] -> [UGen]
        convs inBus js = mceChannels $ convolution2 inputs buffers 1 512 where
                inputs = in' (length js) AR inBus
                --inputs = mce $ map (\_->  whiteNoise 'a' AR) js
                buffers = mce $ map (\i-> control KR ("buf_"++show i) 0.0) js


irDir :: String
irDir = "irs/"

filenames :: [String]
filenames = fmap (\i -> "ir"++show i++ ".wav") is

paths ::FilePath -> [ FilePath ]
paths baseDir = fmap ( (baseDir++irDir)++) filenames

loop :: Server ()
loop = do
        liftIO $ putStrLn "Enter q to quit:" >> hFlush stdout
        s <- liftIO getLine
        when (s /= "q") loop
        
pauseThread :: Double -> Server ()
pauseThread = liftIO . OSC.pauseThread

statusLoop :: Server b
statusLoop = do
    statusM >>= liftIO . print
    pauseThread 1
    statusLoop

serverAction :: FilePath -> Server ()
serverAction baseDir =  do
        --exec_ $ dumpOSC TextPrinter
        r <- rootNode
        resultBs <- exec_ $ do
                buffers <- sequence $ fmap (\p -> b_allocRead p Nothing Nothing) $ paths baseDir
                binfos <- sequence $ map b_query buffers
                sd <- d_recv "filt" def
                let
                        withIndexes = zip is buffers
                        args = fmap (\(i, buf) -> ("buf_" ++ show i, fromIntegral $ bufferId buf) ) withIndexes
                _ <- s_new sd AddToTail r args
                return binfos
        binfos <- extract $ sequence resultBs
        _ <- liftIO $ sequence $ map (\(i,b) -> putStrLn $ "Buffer "++show i++": " ++ show b) (zip is binfos)
        _ <- fork statusLoop
        loop

main :: IO ()
main = do
        (dir,_) <- splitExecutablePath
        --let dir = "/Users/miguelnegrao/Development/Haskell/projects/soniclab-filters/dist/build/soniclab-filters/"
        putStrLn $ "Execution directory: "++show dir
        putStrLn "started sonic lab filters program"
        hFlush stdout
        run dir $ serverAction dir
        putStrLn "stopping sonic lab filters program"
        hFlush stdout
