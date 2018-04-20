using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace smxviewer
{
    public delegate void DrawNodeFn();

    public class NodeData
    {
        public DrawNodeFn callback;
        public object data;

        public NodeData(DrawNodeFn aCallback, object aData)
        {
            callback = aCallback;
            data = aData;
        }
    }
}
