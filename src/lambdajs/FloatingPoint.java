package lambdajs;

import org.rascalmpl.value.IString;
import org.rascalmpl.value.IValue;
import org.rascalmpl.value.IValueFactory;

public class FloatingPoint {
	private final IValueFactory vf;
	public FloatingPoint(IValueFactory vf) {
		this.vf = vf;
	}
    public IValue addFP(IString s1, IString s2) {
    	double v = Double.parseDouble(s1.getValue()) + Double.parseDouble(s2.getValue());
    	return vf.string(Double.toString(v));
    };
    public IValue subFP(IString s1, IString s2) {
    	double v = Double.parseDouble(s1.getValue()) - Double.parseDouble(s2.getValue());
    	return vf.string(Double.toString(v));
    };
    public IValue mulFP(IString s1, IString s2) {
    	double v = Double.parseDouble(s1.getValue()) * Double.parseDouble(s2.getValue());
    	return vf.string(Double.toString(v));
    };
    public IValue divFP(IString s1, IString s2) {
    	double v = Double.parseDouble(s1.getValue()) / Double.parseDouble(s2.getValue());
    	return vf.string(Double.toString(v));
    };
    public IValue negFP(IString s) {
    	double v = -Double.parseDouble(s.getValue());
    	return vf.string(Double.toString(v));
    };
    public IValue ltFP(IString s1, IString s2) {
    	return vf.bool(Double.parseDouble(s1.getValue()) < Double.parseDouble(s2.getValue()));
    }
};