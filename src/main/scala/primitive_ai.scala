
class PrimitiveAI extends SynchPlayer
{
    override def synchPlay : (Int,Int,Int,Int) =
    {
        Thread.sleep(1000)
        for (i<-0 to 7)
        {
            for (j<-0 to 7)
            {
                if (game.canMove(i,j))
                {
                     for (k<-0 to 7)
                    {
                        for (l<-0 to 7)
                        {
                            if (game.canMove(i,j,k,l))
                                return (i,j,k,l)
                        }
                    }
                }
            }
        }
        return (-1,-1,-1,-1)
    }
}