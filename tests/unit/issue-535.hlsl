[numthreads(8, 8, 1)]
void main(uint3 DTID : SV_DispatchThreadID)
{
	uint2 Dimensions;
	Final.GetDimensions(Dimensions.x, Dimensions.y);
	
	// TInput0 - Albedo/Alpha
	// TInput1 - Roughness/Metallic/AO
	// TInput2 - Normalmap
	
	const bool HasAlbedo	= IInput0.x != 0;
	const bool HasAlpha		= IInput1.x != 0;
	const bool HasRoughness = IInput2.x != 0;
	const bool HasMetallic  = IInput3.x != 0;
	const bool HasAO		= IInput4.x != 0;
	const bool HasNormalmap = IInput5.x != 0;
	
	///////////////////////////////////////////////////////////////////////////
	
	float3 rayPos      = float3(0.0, 2.0, 6.0);
	float  sceneFar    = 20.0;
	
	matrix viewToWorld = viewMatrix(rayPos, float3(0.0, 0.0, 0.0), float3(0.0, 1.0, 0.0));
	float3 viewDir     = rayDirection(45.0, float2(Dimensions.xy), float2(DTID.xy));
    float3 rayDir      = normalize(mul(float4(viewDir.xyz, 0.0), viewToWorld)).xyz;
	float3 lightVector = normalize(float3(0.5, 2.0, 0.5));
	float3 lightColor  = float3(1.0, 1.0, 1.0);
	
	SceneHit Hit   = traceSDF(rayPos, rayDir, 0.0, sceneFar, 0.0025);
	float3   Color = float3(0.0, 0.0, 0.0);
    
    if (Hit.tm < sceneFar)
	{
		float3 pW			= rayPos + rayDir * Hit.tm;
		float3 nN			= normalSDF(pW, 0.005);
		float3 Albedo		= float3(0.25, 0.25, 0.25);
		float  Specular		= 0.5;
		float  ao			= 1.0;
		float  metallic		= 0.0;
		float  roughness	= 1.0;
		float  skyIntensity = 0.15;
		float3 CameraVector = rayDir;

		if (Hit.id == 0)
		{
			float2 UV = float2(-atan2(nN.z, nN.x) / M_PI * 2.0, -asin(nN.y) / M_PI * 2.0);
			
			if (HasAlbedo)
				Albedo = TInput0.SampleLevel(smpLnWrap, UV, 0.0).xyz;

			if (HasNormalmap)
			{
				float3 tN = TInput2.SampleLevel(smpLnWrap, UV, 0.0).xyz*2.0-1.0;
				
				float3 Tg = calculateTangent(nN);
				float3 Bn = normalize(cross(nN, Tg));
					   nN = normalize((Tg.xyz * tN.x) + (Bn.xyz * tN.y) + (nN.xyz * tN.z));
			}

			if (HasRoughness || HasMetallic || HasAO)
			{
				float3 RMAO = TInput1.SampleLevel(smpLnWrap, UV, 0.0).xyz;
			
				roughness = HasRoughness ? RMAO.x : roughness;
				metallic  = HasMetallic  ? RMAO.y : metallic;
				ao		  = HasAO        ? RMAO.z : ao;
			}
		}
		else
		{
			roughness	 = 0.8;
			skyIntensity = 0.5;
		}
		
		float3 SpecularColor = lerp(0.08 * Specular, Albedo, metallic); // 0.08 is derived from shading model
		float3 DiffuseColor  = Albedo - Albedo * metallic;
		float3 V		     =-CameraVector;
		float3 N		     = nN;

		// IBL Enviro
		///////////////////////////////////////////////////////////////////////
		
		float3 IBLDirection = 2.0 * dot(V, N) * N - V; 
		float3 RefIBL		= skyColor(IBLDirection, lightVector, roughness);
			   Color	   += (RefIBL * envBRDFApprox(SpecularColor, roughness, saturate(dot(N, V)))) * ao * skyIntensity;

		// Lighting
		///////////////////////////////////////////////////////////////////////
		
		float3 LightShadow	    = traceSoftShadow(pW, lightVector, 0.01, sceneFar/4.0);
		float  LightAttenuation = ao;

		float3 L      = lightVector;
		float  NoL    = saturate((dot(N, L) + 0.5) / 1.5);
			   Color += lightColor * (NoL * LightAttenuation * LightShadow) * orenNayarShading(DiffuseColor, SpecularColor, max(roughness, 0.04), L, V, N, true);

		///////////////////////////////////////////////////////////////////////
    }
	else
	{
		Color = skyColor(rayDir, lightVector);
	}
	
	///////////////////////////////////////////////////////////////////////////

	Color		   = LinearTosRGB(Color);
	Final[DTID.xy] = float4(Color.x, Color.y, Color.z, 1.0);
	return;
}
